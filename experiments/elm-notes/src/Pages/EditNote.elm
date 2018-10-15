module Pages.EditNote exposing (..)

import Editable
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note


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
    in
        ( { model | edtContent = Editable.map (always newContent) model.edtContent }, Cmd.none )
