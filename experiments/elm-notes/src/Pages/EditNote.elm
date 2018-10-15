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


updateContent newContent model =
    { model | edtContent = Editable.map (always newContent) model.edtContent }
