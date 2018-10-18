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
    | NotFound Collection.Id


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
            Session.getNote id session
                |> Maybe.map (Note.getContent >> Existing id)
                |> Maybe.withDefault (NotFound id)
      }
    , Cmd.none
    )


getNC =
    .session >> .nc


getContent model =
    case model.edit of
        New ->
            Just ""

        Existing id content ->
            Just content

        NotFound id ->
            Nothing


overSession : (Session -> Session) -> Model -> Model
overSession updateFn model =
    { model | session = updateFn model.session }


update message model =
    case message of
        Nop ->
            ( model, Cmd.none )

        NewAdded ( note, nc ) ->
            ( model |> overSession (Session.setNC nc)
            , Session.replaceHref (Href.editNote note.id) model.session
            )

        ContentChanged newContent ->
            case model.edit of
                New ->
                    ( model
                    , Collection.createAndAdd (Note.initWithContent newContent) (getNC model)
                        |> Task.perform NewAdded
                    )

                Existing id content ->
                    ( { model | edit = Existing id newContent }, Cmd.none )

                NotFound id ->
                    ( model, Cmd.none )

        ContentBlurred ->
            ( model, Cmd.none )


view : Model -> Skeleton.Details Msg
view model =
    { title = "New Note"
    , attrs = []
    , kids = [ viewKids model ]
    }


viewKids model =
    getContent model
        |> Maybe.map
            (\content ->
                textarea
                    [ class "pa2 h-100 w-100"
                    , value content
                    , onInput (ContentChanged)
                    , onBlur (ContentBlurred)
                    ]
                    []
            )
        |> Maybe.withDefault (div [] [ text "Note Not Found" ])
