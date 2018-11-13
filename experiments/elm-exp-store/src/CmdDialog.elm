module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, subscriptions, update, view)

import BasicsX exposing (safeModBy)
import Browser.Events
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


type alias Model =
    { query : String, selectedIndex : Int }


init =
    { query = "", selectedIndex = 0 }


getFilteredCommands model =
    [ "foo", "bar", "baz", "Bar Zoo" ]
        |> Simple.Fuzzy.filter identity model.query


computeSelectedIndex model =
    Basics.min model.selectedIndex <| List.length (getFilteredCommands model) - 1


type Msg
    = BackDropClicked DomId
    | AutoFocus
    | FocusResult Focus.FocusResult
    | QueryChanged String
    | OnKeyDown HotKey.Event
    | SelectAction String


type OutMsg
    = Cancel
    | Submit String


subscriptions model =
    Sub.batch [ Browser.Events.onKeyDown (D.map OnKeyDown HotKey.decoder) ]


cycleSelectedIdxBy offset model =
    let
        total =
            List.length (getFilteredCommands model)
    in
    if total > 0 then
        let
            si =
                safeModBy total <| model.selectedIndex + offset
        in
        ( { model | selectedIndex = si }, Cmd.none )

    else
        ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message =
    (case message of
        AutoFocus ->
            addEffect (getQueryInputId >> Focus.attempt FocusResult)
                >> withNoOutMsg

        OnKeyDown ke ->
            case ke of
                ( [], "Escape" ) ->
                    withOutMsg (always Cancel)

                ( [], "ArrowDown" ) ->
                    andThen (cycleSelectedIdxBy 1)
                        >> withNoOutMsg

                ( [], "ArrowUp" ) ->
                    andThen (cycleSelectedIdxBy -1)
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


view : Model -> Html Msg
view model =
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
                , sDiv [] [] (viewCmdList model)
                ]
            ]
        ]


viewCmdList : Model -> List (Html Msg)
viewCmdList model =
    getFilteredCommands model |> List.indexedMap (\idx -> viewCmd (idx == computeSelectedIndex model))


viewCmd isSelected cmdName =
    sDiv
        [ if isSelected then
            bg "lightblue"

          else
            bg "inherit"
        ]
        [ class "pa2", onClick <| SelectAction cmdName ]
        [ text cmdName ]
