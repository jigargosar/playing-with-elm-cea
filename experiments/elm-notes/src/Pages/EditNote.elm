module Pages.EditNote exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note
import Process
import Task
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
        ContentChanged newContent ->
            let
                ( newContentInput, cmd ) =
                    model.contentInput |> UserInput.onChange SaveIfDirty newContent
            in
                ( { model | contentInput = newContentInput }
                , cmd
                )
                    |> withoutReply

        SaveIfDirty ->
            let
                ( wasDirty, newContentInput ) =
                    UserInput.onThrottledSaveMsg model.contentInput
            in
                ( { model | contentInput = newContentInput }
                , Cmd.none
                )
                    |> withReply (\m -> maybeBool wasDirty <| SaveContent m.note (content m))
