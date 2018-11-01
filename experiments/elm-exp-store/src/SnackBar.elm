module SnackBar exposing (Msg(..), SnackBar, SnackBarTitle, update)

import BasicsX exposing (..)
import Log
import Process
import Task
import UpdateReturn exposing (..)


type alias SnackBarTitle =
    String


type alias SnackBar =
    { title : SnackBarTitle, maybeProcessId : Maybe Process.Id }


type Msg
    = NoOp
    | Show SnackBarTitle
    | Warn Log.Line


update : Msg -> SnackBar -> ( SnackBar, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

        Show title ->
            pure { model | title = title }
                |> addCmd
                    (model.maybeProcessId
                        |> unwrapMaybe Cmd.none
                            (Process.kill
                                >> Task.attempt (unpackResult (\_ -> Warn [ "Kill Failed" ]) (\_ -> NoOp))
                            )
                    )
