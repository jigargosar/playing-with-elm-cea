module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, subscriptions, update, view)

import Array
import BasicsX exposing (safeModBy)
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
import HotKey
import Html exposing (input)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events exposing (onInput)
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
    { query : String, selectedIndex : Int }


init =
    { query = "", selectedIndex = 0 }


createContextCommand : { x | name : String, id : ContextId } -> Command
createContextCommand { name, id } =
    { name = name, prefix = "Context", outMsg = GotoContextTodoList id, searchText = name }


getFilteredCommands contextStore model =
    let
        commands =
            ContextStore.listActive contextStore
                |> List.map createContextCommand
                |> (::) (createContextCommand ContextStore.inbox)

        simpleMatch config separators needle hay =
            Fuzzy.match config separators needle hay
    in
    commands
        |> List.map (\command -> ( simpleMatch [] [] model.query command.searchText, command ))
        |> List.sortBy (Tuple.first >> .score)


computeSelectedIndex contextStore model =
    Basics.min model.selectedIndex <| List.length (getFilteredCommands contextStore model) - 1


type Msg
    = BackDropClicked
    | AutoFocus
    | FocusResult Focus.FocusResult
    | QueryChanged String
    | GlobalKeyDown HotKey.Event
    | QueryInputKeyDown HotKey.Event
    | SelectCommand Command


type OutMsg
    = Cancel
    | GotoContextTodoList ContextId


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map GlobalKeyDown HotKey.decoder)
        ]


cycleSelectedIdxBy contextStore offset model =
    let
        total =
            List.length (getFilteredCommands contextStore model)
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
                    withOutMsg (always Cancel)

                ( [], "ArrowDown" ) ->
                    andThen (cycleSelectedIdxBy contextStore 1)
                        >> withNoOutMsg

                ( [], "ArrowUp" ) ->
                    andThen (cycleSelectedIdxBy contextStore -1)
                        >> withNoOutMsg

                _ ->
                    withNoOutMsg

        QueryInputKeyDown ke ->
            case ke of
                ( [], "Enter" ) ->
                    withMaybeOutMsg
                        (\model ->
                            getFilteredCommands contextStore model
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
            mapModel (\model -> { model | query = query })
                >> withNoOutMsg

        BackDropClicked ->
            withOutMsg (always <| Cancel)

        SelectCommand command ->
            withOutMsg (always <| command.outMsg)
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
    {- Element.Lazy.lazy2 -}
    viewLazy



--viewLazy : ContextStore -> Model -> Element Msg


viewLazy contextStore windowSize model =
    Elements.viewModal
        { onDismiss = Just BackDropClicked
        , attrs =
            [ Element.alignTop
            , Element.moveDown (toFloat windowSize.height / 100 * 20)
            ]
        , content =
            Element.column [ Elements.spacing2, Element.width <| Element.minimum 400 <| Element.shrink ]
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
    getFilteredCommands contextStore model
        |> List.indexedMap (\idx -> viewCmd (idx == computeSelectedIndex contextStore model))
        |> Elements.list


viewCmd isSelected ( result, command ) =
    Elements.listItem isSelected
        [{- class "pa2", onClick <| SelectCommand command -}]
        [ Elements.tag command.prefix
        , viewFuzzyString result command.name

        --        , sDiv [] [] [ text <| Debug.toString result ]
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
    --    sDiv []
    --        []
    --        [ {- span
    --                 [ style "color" "red"
    --                 ]
    --                 [ text (String.fromInt result.score ++ " ") ]
    --             ,
    --          -}
    --          span [] (Tuple.first highlight)
    --        ]
    Elements.highlightedChars isKey str
