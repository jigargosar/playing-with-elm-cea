module SnackBar exposing (Msg, SnackBar, SnackBarTitle, empty, show, update, view)

import BasicsX exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Log
import Process
import Task
import UI exposing (..)
import UpdateReturn exposing (..)


type alias SnackBarTitle =
    String


type alias SnackBar =
    { title : SnackBarTitle, visible : Bool, maybeProcessId : Maybe Process.Id }


type Msg
    = NoOp
    | Show SnackBarTitle
    | Close
    | SetVisible Bool
    | Warn Log.Line
    | Kill
    | Sleep


empty =
    SnackBar "" False Nothing


show =
    Show


andThenUpdate msg =
    andThen (update msg)


update : Msg -> SnackBar -> ( SnackBar, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

        Kill ->
            ( model
            , unwrapMaybe
                Cmd.none
                (Process.kill
                    >> Task.attempt (unpackResult (\_ -> Warn [ "Kill Failed" ]) (\_ -> NoOp))
                )
                model.maybeProcessId
            )

        Sleep ->
            ( model
            , Process.sleep (1000 * 5)
                |> Task.attempt
                    (unpackResult (\_ -> Warn [ "Sleep Failed" ]) (\_ -> SetVisible False))
            )

        SetVisible visible ->
            pure { model | visible = visible }

        Show title ->
            pure { model | title = title }
                |> andThenUpdate (SetVisible True)
                |> andThenUpdate Kill
                |> andThenUpdate Sleep

        Close ->
            pure model
                |> andThenUpdate (SetVisible False)
                |> andThenUpdate Kill


type alias Config msg =
    { actions : List ( String, msg )
    }


defaultSnackBarConfig =
    Config


view : (Msg -> msg) -> Config msg -> SnackBar -> Html msg
view toMsg config model =
    boolHtml model.visible (viewSnackBar toMsg config model)


viewSnackBar : (Msg -> msg) -> Config msg -> SnackBar -> Html msg
viewSnackBar toMsg config model =
    row "w-100 absolute  bottom-0 z-2 justify-center "
        []
        [ row "bg-black white pa3"
            []
            [ txt model.title
            , button [ onClick <| toMsg Close ] [ text "close" ]
            ]
        ]
