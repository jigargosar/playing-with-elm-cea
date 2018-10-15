module Pages.EditNote exposing (..)

import Editable
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note
import Process
import Task


type alias Model =
    { note : Note.Note, edtContent : Note.EditableContent, isASScheduled : Bool }


init note =
    Model note (Note.getEditableContent note) False


content =
    .edtContent >> Editable.value


getNote =
    .note


type alias F a =
    a -> a


updateContent : { autoSaveMsg : msg } -> String -> Model -> ( Model, Cmd msg )
updateContent { autoSaveMsg } newContent model =
    let
        newEdtContent =
            Editable.map (always newContent) model.edtContent

        isDirty =
            Editable.isDirty newEdtContent

        ( newIsScheduled, cmd ) =
            if model.isASScheduled || not isDirty then
                ( model.isASScheduled, Cmd.none )
            else
                ( True
                , Process.sleep 3000
                    |> Task.perform (always autoSaveMsg)
                )
    in
        ( { model
            | edtContent = Editable.map (always newContent) model.edtContent
            , isASScheduled = newIsScheduled
          }
        , cmd
        )


updateOnAutoSaveMsg model =
    { model
        | edtContent = Editable.save model.edtContent |> Editable.edit
        , isASScheduled = False
    }
