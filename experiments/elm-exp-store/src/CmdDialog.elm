module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, subscriptions, update, view)

import BasicsX exposing (safeModBy)
import Browser.Events
import ContextStore exposing (Context, ContextStore)
import Css exposing (absolute, left, none, pct, pointerEvents, pointerEventsAll, position, right, top, zero)
import DomX exposing (DomId)
import Focus
import HotKey
import Html.Styled exposing (Html, div, input, text)
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
    Context


type alias Model =
    { query : String, selectedIndex : Int }


init =
    { query = "", selectedIndex = 0 }


getFilteredCommands contextStore model =
    ContextStore.list contextStore
        |> Simple.Fuzzy.filter .name model.query


computeSelectedIndex contextStore model =
    Basics.min model.selectedIndex <| List.length (getFilteredCommands contextStore model) - 1


type Msg
    = BackDropClicked DomId
    | AutoFocus
    | FocusResult Focus.FocusResult
    | QueryChanged String
    | OnKeyDown HotKey.Event
    | SelectAction Command


type OutMsg
    = Cancel
    | Submit Command


subscriptions model =
    Sub.batch [ Browser.Events.onKeyDown (D.map OnKeyDown HotKey.decoder) ]


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

        OnKeyDown ke ->
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

        SelectAction action ->
            withOutMsg (always <| Submit action)
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

                        --                                                  , HotKey.onKeyDown ContentInputKeyDown
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


viewCmd isSelected context =
    let
        nameString =
            context.name
    in
    sDiv
        [ if isSelected then
            bg "lightblue"

          else
            bg "inherit"
        ]
        [ class "pa2", onClick <| SelectAction context ]
        [ text nameString ]
