module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, subscriptions, update, view)

import Array
import BasicsX exposing (safeModBy)
import Browser.Events
import ContextStore exposing (Context, ContextId, ContextStore)
import Css exposing (absolute, left, none, pct, pointerEvents, pointerEventsAll, position, right, top, zero)
import DomX exposing (DomId)
import Focus
import Fuzzy
import HotKey
import Html.Styled exposing (Html, div, input, span, styled, text)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
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

        --        _ =
        --            commands
        --                |> List.map (\command -> ( simpleMatch [] [] model.query command.searchText, command ))
        --                |> Debug.log "res"
        _ =
            commands
                |> Simple.Fuzzy.filter .searchText model.query
    in
    commands
        |> List.map (\command -> ( simpleMatch [] [] model.query command.searchText, command ))
        |> List.sortBy (Tuple.first >> .score)


computeSelectedIndex contextStore model =
    Basics.min model.selectedIndex <| List.length (getFilteredCommands contextStore model) - 1


type Msg
    = BackDropClicked DomId
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
    Sub.batch [ Browser.Events.onKeyDown (D.map GlobalKeyDown HotKey.decoder) ]


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

        BackDropClicked targetId ->
            withMaybeOutMsg
                (\model ->
                    if targetId == getBackdropDomId model then
                        Just Cancel

                    else
                        Nothing
                )

        SelectCommand command ->
            withOutMsg (always <| command.outMsg)
    )
        << pure


getDomIdPrefix model =
    "cmd-dialog"


getBackdropDomId =
    getDomIdPrefix >> (++) "-backdrop"


getQueryInputId =
    getDomIdPrefix >> (++) "-query-input"


view : ContextStore -> Model -> Html Msg
view contextStore model =
    div []
        [ UI.backdrop [ id <| getBackdropDomId model, DomX.onClickTargetId BackDropClicked ]
            []
        , sDiv [ position absolute, absFill, rowCXY, pointerEvents none ]
            []
            [ sDiv [ position absolute, top (pct 10), pointerEventsAll ]
                [ class "bg-white br4 shadow-1 pa3 measure w-100"
                ]
                [ sDiv [ vs, w100, rowCY ]
                    []
                    [ input
                        [ id <| getQueryInputId model
                        , placeholder "Type Command Name"
                        , class "flex-auto pa3"
                        , value model.query
                        , onInput QueryChanged
                        , HotKey.onKeyDown QueryInputKeyDown
                        ]
                        []
                    ]
                , sDiv [] [] (viewCmdList contextStore model)
                ]
            ]
        ]


viewCmdList : ContextStore -> Model -> List (Html Msg)
viewCmdList contextStore model =
    getFilteredCommands contextStore model
        |> List.indexedMap (\idx -> viewCmd (idx == computeSelectedIndex contextStore model))


viewCmd isSelected ( result, command ) =
    let
        _ =
            Debug.log "match" result
    in
    sDiv
        [ rowCY
        , if isSelected then
            bg "lightblue"

          else
            bg "inherit"
        ]
        [ class "pa2", onClick <| SelectCommand command ]
        [ sDiv [] [] [ viewFuzzyString result command.name ]
        , sDiv [] [] [ text <| Debug.toString result ]
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
    in
    span [] []
