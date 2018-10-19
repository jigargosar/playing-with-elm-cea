module Pages.Note exposing (..)

import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Note exposing (Note)
import NotesCollection exposing (NotesCollection)
import Session exposing (Session)
import Skeleton
import Task
import UI exposing (link)


type Edit
    = New
    | Editing Note.Id Note.Content
    | Viewing Note.Id Note.Content
    | NotFound Note.Id


type alias Model =
    { session : Session, edit : Edit }


type Msg
    = Nop
    | ContentChanged Note.Content
    | ContentBlurred
    | NewAdded ( ( Note, NotesCollection ), Cmd Msg )
    | UpdateCollection ( NotesCollection, Cmd Msg )


initNewNote : Session -> ( Model, Cmd Msg )
initNewNote session =
    ( { session = session, edit = New }, Cmd.none )


initEditNote : Note.Id -> Session -> ( Model, Cmd Msg )
initEditNote id session =
    ( { session = session
      , edit =
            Session.getNote id session
                |> Maybe.map (Note.getContent >> Editing id)
                |> Maybe.withDefault (NotFound id)
      }
    , Cmd.none
    )


initShowNote : Note.Id -> Session -> ( Model, Cmd Msg )
initShowNote id session =
    ( { session = session
      , edit =
            Session.getNote id session
                |> Maybe.map (Note.getContent >> Viewing id)
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

        Editing id content ->
            Just content

        Viewing id content ->
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

        NewAdded ( ( note, nc ), cmd ) ->
            ( model |> overSession (Session.setNC nc)
            , Cmd.batch [ cmd, Session.replaceHref (Href.editNoteId note.id) model.session ]
            )

        UpdateCollection ( nc, cmd ) ->
            ( model |> overSession (Session.setNC nc)
            , cmd
            )

        ContentChanged newContent ->
            case model.edit of
                New ->
                    ( model
                    , NotesCollection.addNewWithContent newContent (getNC model)
                        |> Task.perform NewAdded
                    )

                Editing id content ->
                    ( { model | edit = Editing id newContent }
                    , getNC model |> NotesCollection.updateNoteContent id content |> Task.perform UpdateCollection
                    )

                NotFound id ->
                    ( model, Cmd.none )

                Viewing id content ->
                    ( model, Cmd.none )

        ContentBlurred ->
            ( model, Cmd.none )


view : Model -> Skeleton.Details Msg
view model =
    { title = getPageTitle model
    , attrs = []
    , kids = [ viewKids model ]
    }


getPageTitle model =
    case model.edit of
        New ->
            "New Note"

        Editing _ _ ->
            "Edit Note"

        Viewing _ _ ->
            "View Note"

        NotFound _ ->
            "Oops"


viewKids model =
    case model.edit of
        New ->
            noteEditor ""

        Editing id content ->
            div [ class "flex flex-column h-100 vs3" ]
                [ link (Href.viewNoteId id) "View"
                , noteEditor content
                ]

        Viewing id content ->
            div []
                [ a [ href <| Href.editNoteId id ] [ text "Edit" ]
                , div [ class " pv2 " ] <| Markdown.toHtml Nothing content
                ]

        NotFound id ->
            div [] [ text "Note Not Found" ]


noteEditor content =
    div [ class "flex-auto flex" ]
        [ textarea
            [ class "pa2 min-h-100 w-100"
            , value content
            , onInput (ContentChanged)
            , onBlur (ContentBlurred)
            ]
            []
        ]
