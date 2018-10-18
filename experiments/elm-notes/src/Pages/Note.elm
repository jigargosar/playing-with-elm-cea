module Pages.Note exposing (..)

import Collection
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note exposing (Note)
import Session exposing (NotesCollection, Session)
import Skeleton
import Task


type alias Model =
    { session : Session }


type Msg
    = Nop


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


getNC =
    .session >> .nc


overSession : (Session -> Session) -> Model -> Model
overSession updateFn model =
    { model | session = updateFn model.session }


update message model =
    case message of
        Nop ->
            ( model, Cmd.none )



--        NewAdded ( note, nc ) ->
--            ( model |> overSession (Session.setNC nc), Session.pushHref ("note/" ++ note.id) model.session )
--
--        New ->
--            ( model
--            , Collection.createAndAdd (Note.init) (getNC model)
--                |> Task.perform NewAdded
--            )


view : Model -> Skeleton.Details Msg
view model =
    { title = "New Note"
    , attrs = []
    , kids =
        []
    }
