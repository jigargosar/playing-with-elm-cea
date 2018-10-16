module Pages.EditNote exposing (..)

import Editable
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note
import Process
import Task
import Throttle


type alias Model =
    { note : Note.Note
    , edtContent : Note.EditableContent
    , throttleSave : Throttle.Model
    , isASScheduled : Bool
    }


init note =
    Model note
        (Note.getEditableContent note)
        (Throttle.init 3000)
        False


content =
    .edtContent >> Editable.value


getNote =
    .note


type alias NoteContent =
    String


type Msg
    = AutoSave
    | ContentChanged NoteContent
    | SetContent NoteContent


type Reply
    = SaveContent Note.Note NoteContent


withReply f ( m, c ) =
    ( m, c, f m )


withoutReply ( m, c ) =
    ( m, c, Nothing )


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
        ( m2, Cmd.batch [ c1, c2 ] )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Reply )
update msg model =
    case msg of
        AutoSave ->
            ( { model
                | edtContent = Editable.save model.edtContent
                , isASScheduled = False
              }
            , Cmd.none
            )
                |> withReply (\m -> Just <| SaveContent m.note (content m))

        SetContent newContent ->
            ( model, Cmd.none )
                |> withoutReply

        ContentChanged newContent ->
            ( { model
                | edtContent =
                    model.edtContent
                        |> Editable.edit
                        |> Editable.map (always newContent)
              }
            , Cmd.none
            )
                |> andThen updateScheduleSave
                |> withoutReply


updateScheduleSave model =
    let
        ( newIsScheduled, cmd ) =
            if model.isASScheduled then
                ( model.isASScheduled, Cmd.none )
            else
                ( True
                , Process.sleep 3000
                    |> Task.perform (always AutoSave)
                )
    in
        ( { model
            | isASScheduled = newIsScheduled
          }
        , cmd
        )
