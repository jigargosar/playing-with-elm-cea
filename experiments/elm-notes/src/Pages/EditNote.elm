module Pages.EditNote exposing
    ( ContentInput
    , Model
    , Msg(ContentBlurred, ContentChanged)
    , NoteContent
    , content
    , init
    , subscriptions
    , update
    )

import Basics.Extra exposing (flip)
import Collection
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Note
import Ports
import Process
import Task exposing (Task)
import Time
import UserInput


type alias ContentInput =
    UserInput.Model NoteContent


type alias Model =
    { note : Note.Note
    , contentInput : ContentInput
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
    | PersistNote Note.Note
    | UpdateNoteFromNotesCollectionChanges E.Value


subscriptions =
    Sub.batch [ Ports.notesCollectionChanged UpdateNoteFromNotesCollectionChanges ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PersistNote note ->
            ( model, Ports.persistNoteList <| Note.encode note )

        UpdateNoteFromNotesCollectionChanges encodedNotesCollection ->
            let
                newNote =
                    D.decodeValue (D.dict Note.decoder) encodedNotesCollection
                        |> Result.toMaybe
                        |> Maybe.andThen (Dict.get model.note.id >> Debug.log "Note Updated")
                        |> Maybe.withDefault model.note
            in
            ( { model | note = newNote }, Cmd.none )

        ContentChanged newContent ->
            model.contentInput
                |> UserInput.onChange ThrottledSave newContent
                |> Tuple.mapFirst (flip setContentInput model)

        Save ->
            ( model
            , {- taskNowMilli
                 |> Task.map (Note.updateContent (content model) model.note)
                 |> Task.perform PersistNote
              -}
              Cmd.none
            )

        ContentBlurred ->
            update Save (overContentInput UserInput.save model)

        ThrottledSave ->
            update Save (overContentInput UserInput.onThrottledSaveMsg model)


taskNowMilli : Task x Int
taskNowMilli =
    Time.now |> Task.map Time.posixToMillis


overContentInput : (ContentInput -> ContentInput) -> Model -> Model
overContentInput updateFn model =
    { model | contentInput = updateFn model.contentInput }


setContentInput : ContentInput -> Model -> Model
setContentInput =
    always >> overContentInput
