module Pages.Note exposing (..)

import Collection
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note exposing (Note)
import Session exposing (NotesCollection, Session)
import Skeleton
import Task


type Edit
    = New
    | Existing Collection.Id Note.Content


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


initWithNoteId : Collection.Id -> Session -> ( Model, Cmd Msg )
initWithNoteId id session =
    ( { session = session
      , edit =
            Existing id
                (session.nc
                    |> Collection.get id
                    |> Maybe.map Note.getContent
                    |> Maybe.withDefault "Note Not Found ;("
                )
      }
    , Cmd.none
    )


getNC =
    .session >> .nc


getContent model =
    case model.edit of
        New ->
            ""

        Existing id content ->
            content


overSession : (Session -> Session) -> Model -> Model
overSession updateFn model =
    { model | session = updateFn model.session }


update message model =
    case message of
        Nop ->
            ( model, Cmd.none )

        NewAdded ( note, nc ) ->
            ( model |> overSession (Session.setNC nc), Session.replaceHref (Href.editNote note.id) model.session )

        ContentChanged newContent ->
            case model.edit of
                New ->
                    ( model
                    , Collection.createAndAdd (Note.initWithContent newContent) (getNC model)
                        |> Task.perform NewAdded
                    )

                Existing id content ->
                    ( { model | edit = Existing id newContent }, Cmd.none )

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
