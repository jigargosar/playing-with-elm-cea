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


type Msg
    = AutoSave
    | ContentChanged String


type Reply
    = SaveContent Note.Note String


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


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Reply )
update msg model =
    case msg of
        AutoSave ->
            let
                newEdtContent =
                    Editable.save model.edtContent |> Editable.edit
            in
                ( { model
                    | edtContent = Editable.save model.edtContent |> Editable.edit
                    , isASScheduled = False
                  }
                , Cmd.none
                , Just <| SaveContent model.note (Editable.value newEdtContent)
                )

        ContentChanged newContent ->
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
                            |> Task.perform (always AutoSave)
                        )
            in
                ( { model
                    | edtContent = Editable.map (always newContent) model.edtContent
                    , isASScheduled = newIsScheduled
                  }
                , cmd
                , Nothing
                )
