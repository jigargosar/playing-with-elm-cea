module Pages.Note exposing (..)

import Collection
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note exposing (Note)
import Session exposing (NotesCollection, Session)
import Skeleton
import Task


type Edit
    = New
    | Existing


type alias Model =
    { session : Session, edit : Edit }


type Msg
    = Nop
    | ContentChanged Note.Content
    | ContentBlurred
    | NewAdded ( Note, NotesCollection )


initNewNote : Session -> ( Model, Cmd Msg )
initNewNote session =
    ( { session = session, edit = New }, Cmd.none )


getNC =
    .session >> .nc


getContent model =
    case model.edit of
        New ->
            ""

        Existing ->
            ""


overSession : (Session -> Session) -> Model -> Model
overSession updateFn model =
    { model | session = updateFn model.session }


update message model =
    case message of
        Nop ->
            ( model, Cmd.none )

        NewAdded ( note, nc ) ->
            ( model, Cmd.none )

        ContentChanged newContent ->
            case model.edit of
                New ->
                    ( model
                    , Collection.createAndAdd (Note.initWithContent newContent) (getNC model)
                        |> Task.perform NewAdded
                    )

                Existing ->
                    ( model, Cmd.none )

        ContentBlurred ->
            ( model, Cmd.none )


view : Model -> Skeleton.Details Msg
view model =
    { title = "New Note"
    , attrs = []
    , kids =
        [ textarea
            [ class "pa2 h-100 w-100"
            , value <| getContent model
            , onInput (ContentChanged)
            , onBlur (ContentBlurred)
            ]
            []
        ]
    }
