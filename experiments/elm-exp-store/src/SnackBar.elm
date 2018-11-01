module SnackBar exposing (Msg(..), SnackBar, SnackBarTitle, empty, update)

import BasicsX exposing (..)
import Log
import Process
import Task
import UpdateReturn exposing (..)


type alias SnackBarTitle =
    String


type alias SnackBar =
    { title : SnackBarTitle, visible : Bool, maybeProcessId : Maybe Process.Id }


type Msg
    = NoOp
    | Show SnackBarTitle
    | SetVisible Bool
    | Warn Log.Line
    | Kill
    | Sleep


empty =
    SnackBar "" False Nothing


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
