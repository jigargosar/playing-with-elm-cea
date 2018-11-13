module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, subscriptions, update, view)

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
    { query : String }


init =
    { query = "" }


type Msg
    = BackDropClicked DomId
    | AutoFocus
    | FocusResult Focus.FocusResult
    | QueryChanged String
    | OnKeyDown HotKey.Event


type OutMsg
    = Cancel


subscriptions model =
    Sub.batch [ Browser.Events.onKeyDown (D.map OnKeyDown HotKey.decoder) ]


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
    [ "foo", "bar", "baz", "Bar Zoo" ]
        |> Simple.Fuzzy.filter identity model.query
        |> List.map viewCmd


viewCmd cmdName =
    sDiv [] [] [ text cmdName ]
