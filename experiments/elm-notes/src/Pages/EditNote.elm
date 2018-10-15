module Pages.EditNote exposing (..)

import Editable
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note
import Process
import Task


type alias Model =
    { note : Note.Note, edtContent : Note.EditableContent }


init note =
    Model note (Note.getEditableContent note)


content =
    .edtContent >> Editable.value


type alias F a =
    a -> a


updateContent : { autoSaveMsg : msg } -> String -> Model -> ( Model, Cmd msg )
updateContent { autoSaveMsg } newContent model =
    let
        wasDirty =
            Editable.isDirty model.edtContent

        cmd =
            if wasDirty then
                Cmd.none
            else
                Process.sleep 3000
                    |> Task.perform (always autoSaveMsg)
    in
        ( { model | edtContent = Editable.map (always newContent) model.edtContent }, cmd )
