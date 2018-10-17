module Pages.EditNote exposing (..)

import Collection
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note
import Ports
import Process
import Task exposing (Task)
import Time
import UserInput
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { note : Note.Note
    , contentInput : UserInput.Model NoteContent
    }


init note =
    { note = note
    , contentInput = Note.getContent note |> UserInput.init
    }


content =
    .contentInput >> UserInput.get


getNote =
    .note


type alias NoteContent =
    String


type Msg
    = ContentChanged NoteContent
    | ThrottledSave
    | ContentBlurred
    | Save
    | SetNoteAndPersist Note.Note
    | UpdateNoteFromNotesCollectionChanges E.Value


subscriptions =
    Sub.batch [ Ports.notesCollectionChanged UpdateNoteFromNotesCollectionChanges ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNoteAndPersist note ->
            ( model, Ports.persistNote <| Note.encode note )

        UpdateNoteFromNotesCollectionChanges encodedNotesCollection ->
            let
                newNote =
                    D.decodeValue (D.dict Note.decoder) encodedNotesCollection
                        |> Result.toMaybe
                        |> Maybe.andThen (Dict.get model.note.id >> Debug.log "Note Updated")
                        |> Maybe.withDefault (model.note)
            in
                ( { model | note = newNote }, Cmd.none )

        ContentChanged newContent ->
            let
                ( newContentInput, cmd ) =
                    model.contentInput |> UserInput.onChange ThrottledSave newContent
            in
                ( { model | contentInput = newContentInput }
                , cmd
                )

        Save ->
            ( model
            , taskNowMilli
                |> Task.map (\now -> Note.updateContent (content model) now model.note)
                |> Task.perform SetNoteAndPersist
            )

        ContentBlurred ->
            update Save { model | contentInput = UserInput.save model.contentInput }

        ThrottledSave ->
            update Save { model | contentInput = UserInput.onThrottledSaveMsg model.contentInput }


taskNowMilli : Task x Int
taskNowMilli =
    Time.now |> Task.map Time.posixToMillis
