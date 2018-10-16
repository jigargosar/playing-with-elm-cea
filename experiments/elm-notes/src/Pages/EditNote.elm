module Pages.EditNote exposing (..)

import EditableInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note
import Process
import Task
import Throttle


type alias Model =
    { note : Note.Note
    , edtContent : EditableInput.Model String
    , throttleSave : Throttle.Model
    }


init note =
    { note = note
    , edtContent = Note.getContent note |> EditableInput.init
    , throttleSave = Throttle.init 3000
    }


content =
    .edtContent >> EditableInput.get


isContentDirty =
    .edtContent >> EditableInput.dirty


type alias NoteContent =
    String


type Msg
    = ContentChanged NoteContent
    | SaveIfDirty


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


maybeBool bool value =
    if bool then
        Just value
    else
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Reply )
update msg model =
    case msg of
        SaveIfDirty ->
            let
                wasDirty =
                    EditableInput.dirty model.edtContent
            in
                ( { model
                    | edtContent = EditableInput.save model.edtContent
                    , throttleSave = Throttle.updateOnEmit model.throttleSave
                  }
                , Cmd.none
                )
                    |> withReply (\m -> maybeBool wasDirty <| SaveContent m.note (content m))

        ContentChanged newContent ->
            let
                ( newThrottleSave, cmd ) =
                    Throttle.push SaveIfDirty model.throttleSave
            in
                ( { model
                    | edtContent = model.edtContent |> EditableInput.set newContent
                    , throttleSave = newThrottleSave
                  }
                , cmd
                )
                    |> withoutReply
