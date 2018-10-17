module Pages.EditNote exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note
import Ports
import Process
import Task exposing (Task)
import Time
import UserInput


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
    | SetNoteAndPersist Note.Note


type Reply
    = SaveContent Note.Note NoteContent


maybeBool bool value =
    if bool then
        Just value
    else
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Reply )
update msg model =
    case msg of
        SetNoteAndPersist _ ->
            ( model, Cmd.none, Nothing )

        ContentChanged newContent ->
            let
                ( newContentInput, cmd ) =
                    model.contentInput |> UserInput.onChange ThrottledSave newContent
            in
                ( { model | contentInput = newContentInput }
                , cmd
                , Nothing
                )

        ContentBlurred ->
            let
                ( wasDirty, newContentInput ) =
                    UserInput.save model.contentInput

                reply =
                    maybeBool wasDirty <| SaveContent model.note (UserInput.get newContentInput)
            in
                ( { model | contentInput = newContentInput }
                , Cmd.none
                , reply
                )

        ThrottledSave ->
            let
                ( wasDirty, newContentInput ) =
                    UserInput.onThrottledSaveMsg model.contentInput

                reply =
                    maybeBool wasDirty <| SaveContent model.note (UserInput.get newContentInput)
            in
                ( { model | contentInput = newContentInput }
                , Cmd.none
                , reply
                )


update2 : Msg -> Model -> ( Model, Cmd Msg )
update2 msg model =
    case msg of
        SetNoteAndPersist note ->
            ( model, Ports.persistNote <| Note.encode note )

        ContentChanged newContent ->
            let
                ( newContentInput, cmd ) =
                    model.contentInput |> UserInput.onChange ThrottledSave newContent
            in
                ( { model | contentInput = newContentInput }
                , cmd
                )

        ContentBlurred ->
            let
                ( wasDirty, newContentInput ) =
                    UserInput.save model.contentInput
            in
                ( { model | contentInput = newContentInput }
                , taskNowMilli
                    |> Task.map (\now -> Note.updateContent (UserInput.get newContentInput) now model.note)
                    |> Task.perform SetNoteAndPersist
                )

        ThrottledSave ->
            let
                ( wasDirty, newContentInput ) =
                    UserInput.onThrottledSaveMsg model.contentInput
            in
                ( { model | contentInput = newContentInput }
                , Cmd.none
                )


taskNowMilli : Task x Int
taskNowMilli =
    Time.now |> Task.map Time.posixToMillis
