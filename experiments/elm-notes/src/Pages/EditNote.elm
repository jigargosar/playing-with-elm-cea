module Pages.EditNote exposing (..)

import EditableInput exposing (EditableInput)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note
import Process
import Task
import Throttle


type alias Model =
    { note : Note.Note
    , edtContent : EditableInput String
    , throttleSave : Throttle.Model
    , saveScheduled : Bool
    }


init note =
    Model note
        (Note.getContent note |> EditableInput.init)
        (Throttle.init 3000)
        False


content =
    .edtContent >> EditableInput.get


isContentDirty =
    .edtContent >> EditableInput.dirty


type alias NoteContent =
    String


type Msg
    = AutoSave
    | ContentChanged NoteContent
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
        AutoSave ->
            ( { model
                | edtContent = EditableInput.save model.edtContent
                , saveScheduled = False
              }
            , Cmd.none
            )
                |> withReply (\m -> Just <| SaveContent m.note (content m))

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
            ( { model
                | edtContent =
                    model.edtContent
                        |> EditableInput.set newContent
              }
            , Cmd.none
            )
                |> andThen updateThrottleSave
                |> withoutReply


updateThrottleSave model =
    let
        ( newThrottleSave, cmd ) =
            Throttle.push SaveIfDirty model.throttleSave
    in
        ( { model
            | throttleSave = newThrottleSave
          }
        , cmd
        )



--updateCheckAndScheduleSave model =
--    let
--        ( newSaveScheduled, cmd ) =
--            if model.saveScheduled || not (isContentDirty model) then
--                ( model.saveScheduled, Cmd.none )
--            else
--                ( True
--                , Process.sleep 3000
--                    |> Task.perform (always AutoSave)
--                )
--    in
--        ( { model
--            | saveScheduled = newSaveScheduled
--          }
--        , cmd
--        )
