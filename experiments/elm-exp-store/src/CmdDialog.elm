module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, subscriptions, update, view)

import Array
import BasicsX exposing (isWhitespaceOrEmptyString, safeModBy)
import Browser.Events
import ContextStore exposing (Context, ContextId, ContextStore)
import Css exposing (absolute, borderRadius, left, none, pct, pointerEvents, pointerEventsAll, position, px, right, top, zero)
import DomX exposing (DomId)
import Element exposing (Element)
import Element.Events
import Element.Lazy
import Elements
import Focus
import Fuzzy
import HotKey exposing (SoftKey(..))
import Html exposing (input)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events exposing (onInput)
import Html.Lazy
import Json.Decode as D
import Json.Encode as E
import Log
import Simple.Fuzzy
import Styles exposing (..)
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Command =
    { name : String, prefix : String, outMsg : OutMsg, searchText : String }


type alias Model =
    { query : String, selectedIndex : Int, shiftDown : Bool }


init =
    { query = "", selectedIndex = 0, shiftDown = False }


createContextCommand : { x | name : String, id : ContextId } -> Command
createContextCommand { name, id } =
    { name = name, prefix = "Context", outMsg = GotoContextTodoList id, searchText = name }


getFilteredCommandResults contextStore model =
    let
        commands =
            ContextStore.listActive contextStore
                |> List.map createContextCommand
                |> (::) (createContextCommand ContextStore.inbox)

        simpleMatch config separators needle hay =
            Fuzzy.match config separators needle hay

        sortIfQueryNotEmpty =
            if isWhitespaceOrEmptyString model.query then
                identity

            else
                List.sortBy (Tuple.first >> .score)
    in
    commands
        |> List.map (\command -> ( simpleMatch [] [] model.query command.searchText, command ))
        |> sortIfQueryNotEmpty


computeSelectedIndex contextStore model =
    Basics.min model.selectedIndex <| List.length (getFilteredCommandResults contextStore model) - 1


type Msg
    = BackDropClicked
    | AutoFocus
    | FocusResult Focus.FocusResult
    | QueryChanged String
    | GlobalKeyDown HotKey.Event
    | GlobalKeyUp HotKey.Event
    | QueryInputKeyDown HotKey.Event
    | SelectCommand Command


type OutMsg
    = Dismiss
    | GotoContextTodoList ContextId


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map GlobalKeyDown HotKey.decoder)
        , Browser.Events.onKeyUp (D.map GlobalKeyUp HotKey.decoder)
        ]


cycleSelectedIdxBy contextStore offset model =
    let
        total =
            List.length (getFilteredCommandResults contextStore model)
    in
    if total > 0 then
        let
            si =
                safeModBy total <| model.selectedIndex + offset
        in
        ( { model | selectedIndex = si }, Cmd.none )

    else
        ( model, Cmd.none )


update : ContextStore -> Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update contextStore message =
    (case message of
        AutoFocus ->
            addEffect (getQueryInputId >> Focus.attempt FocusResult)
                >> withNoOutMsg

        GlobalKeyDown ke ->
            case ke of
                ( [], "Escape" ) ->
                    withOutMsg Dismiss

                ( [], "ArrowDown" ) ->
                    andThen (cycleSelectedIdxBy contextStore 1)
                        >> withNoOutMsg

                ( [], "ArrowUp" ) ->
                    andThen (cycleSelectedIdxBy contextStore -1)
                        >> withNoOutMsg

                ( _, "Shift" ) ->
                    mapModel (\model -> { model | shiftDown = True })
                        >> withNoOutMsg

                ( _, key ) ->
                    let
                        _ =
                            Debug.log "keyDown" key
                    in
                    withNoOutMsg

        GlobalKeyUp ke ->
            case ke of
                ( _, "Shift" ) ->
                    mapModel (\model -> { model | shiftDown = False })
                        >> withNoOutMsg

                ( _, key ) ->
                    let
                        _ =
                            Debug.log "keyUp" key
                    in
                    withNoOutMsg

        QueryInputKeyDown ke ->
            case ke of
                ( [], "Enter" ) ->
                    withMaybeOutMsg
                        (\model ->
                            getFilteredCommandResults contextStore model
                                |> Array.fromList
                                |> Array.get (computeSelectedIndex contextStore model)
                                |> Maybe.map (Tuple.second >> .outMsg)
                        )

                _ ->
                    withNoOutMsg

        FocusResult r ->
            addCmd (Log.focusResult "CmdDialog.elm" r)
                >> withNoOutMsg

        QueryChanged query ->
            mapModel (\model -> { model | query = query, selectedIndex = 0 })
                >> withNoOutMsg

        BackDropClicked ->
            withOutMsg Dismiss

        SelectCommand command ->
            withOutMsg command.outMsg
    )
        << pure


getDomIdPrefix model =
    "cmd-dialog"


getBackdropDomId model =
    getDomIdPrefix model ++ "-backdrop"


getQueryInputId model =
    getDomIdPrefix model ++ "-query-input"



--view : ContextStore -> Model -> Element Msg


view =
    Html.Lazy.lazy3 viewLazy



--viewLazy : ContextStore -> Model -> Element Msg


viewLazy contextStore windowSize model =
    Elements.viewModal
        { onDismiss = Just BackDropClicked
        , attrs =
            [ Element.alignTop
            , Element.moveDown (toFloat windowSize.height / 100 * 20)
            ]
        , content =
            Element.column
                [ Elements.spacing2
                , Element.width <|
                    Element.minimum (toFloat windowSize.width / 100 * 80 |> round) <|
                        Element.shrink
                ]
                [ Element.el [ Element.width Element.fill ]
                    (input
                        [ id <| getQueryInputId model
                        , placeholder "Type Command Name"
                        , class "flex-auto pa3"
                        , value model.query
                        , onInput QueryChanged
                        , HotKey.onKeyDownHtml QueryInputKeyDown
                        ]
                        []
                        |> Element.html
                    )
                , viewCmdList contextStore model
                ]
        }



--    viewBackdrop model |> Element.inFront (viewBackdrop model)
--        , sDiv [ position absolute, absFill, rowCXY, pointerEvents none ]
--            []
--            [ sDiv [ position absolute, top (pct 10), pointerEventsAll ]
--                [ class "bg-white br4 shadow-1 pa3 measure w-100"
--                ]
--                [ sDiv [ vs, w100, rowCY ]
--                    []
--                    [ input
--                        [ id <| getQueryInputId model
--                        , placeholder "Type Command Name"
--                        , class "flex-auto pa3"
--                        , value model.query
--                        , onInput QueryChanged
--                        , HotKey.onKeyDown QueryInputKeyDown
--                        ]
--                        []
--                    ]
--                , fromElement (viewCmdList contextStore model)
--                ]
--            ]
--        ]


viewCmdList : ContextStore -> Model -> Element Msg
viewCmdList contextStore model =
    getFilteredCommandResults contextStore model
        |> List.indexedMap (\idx -> viewCmd (idx == computeSelectedIndex contextStore model))
        |> Elements.list


viewCmd isSelected ( result, command ) =
    Elements.listItem isSelected
        [{- class "pa2", onClick <| SelectCommand command -}]
        [ Elements.tag command.prefix
        , viewFuzzyString result command.name
        ]


viewFuzzyString result str =
    let
        isKey index =
            List.foldl
                (\e sum ->
                    if not sum then
                        List.member (index - e.offset) e.keys

                    else
                        sum
                )
                False
                result.matches

        isMatch index =
            List.foldl
                (\e sum ->
                    if not sum then
                        e.offset <= index && (e.offset + e.length) > index

                    else
                        sum
                )
                False
                result.matches

        stylesAt index =
            [ if isKey index then
                [ fg "red" ]

              else
                []

            --            , if isMatch index then
            --                [ bg "yellow" ]
            --
            --              else
            --                []
            ]
                |> List.concat

        accumulateChar c ( sum, index ) =
            ( sum ++ [ ( index, c ) ], index + 1 )

        indexCharPair =
            String.foldl accumulateChar ( [], 0 ) str
    in
    Elements.highlightedChars isKey str
