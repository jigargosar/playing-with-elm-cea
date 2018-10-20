module Page.Note exposing (Model, Msg, getContent, initEditNote, initNewNote, initShowNote, update, view)

import FeatherIcons
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Note exposing (Note)
import NotesCollection exposing (NotesCollection)
import Session exposing (Session)
import Skeleton exposing (defaultSkeletonDetails)
import Task
import UI exposing (link, row, spacer)


type Edit
    = New
    | Editing Note.Id Note.Content
    | Viewing Note.Id Note.Content
    | NotFound Note.Id


type alias Model =
    { session : Session, edit : Edit }


type Msg
    = Nop
    | Back
    | ContentChanged Note.Content
    | ContentBlurred
    | NewAdded ( ( Note, NotesCollection ), Cmd Msg )
    | StartEditing Note.Id
    | View Note.Id
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

        Back ->
            ( model, Session.back model.session )

        NewAdded ( ( note, nc ), cmd ) ->
            ( model |> overSession (Session.setNC nc)
            , Cmd.batch [ cmd, Session.replaceHref (Href.editNoteId note.id) model.session ]
            )

        UpdateCollection ( nc, cmd ) ->
            ( model |> overSession (Session.setNC nc)
            , cmd
            )

        StartEditing id ->
            ( model, Session.replaceHref (Href.editNoteId id) model.session )

        View id ->
            ( model, Session.replaceHref (Href.viewNoteId id) model.session )

        ContentChanged newContent ->
            case model.edit of
                New ->
                    ( model
                    , NotesCollection.addNewWithContent newContent (getNC model)
                        |> Task.perform NewAdded
                    )

                Editing id _ ->
                    ( { model | edit = Editing id newContent }
                    , getNC model |> NotesCollection.updateNoteContent id newContent |> Task.perform UpdateCollection
                    )

                NotFound id ->
                    ( model, Cmd.none )

                Viewing id content ->
                    ( model, Cmd.none )

        ContentBlurred ->
            ( model, Cmd.none )


view : Model -> Skeleton.Details Msg
view model =
    { defaultSkeletonDetails
        | title = getPageTitle model
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
            div [ class "flex flex-column h-100 vs3" ]
                [ row ""
                    []
                    [ spacer
                    , button [ onClick <| Back ]
                        [ FeatherIcons.slash
                            |> FeatherIcons.toHtml []
                        ]
                    ]
                , noteEditor ""
                ]

        Editing id content ->
            div [ class "flex flex-column h-100 vs3" ]
                [ row ""
                    []
                    [ spacer
                    , button [ onClick <| View id ]
                        [ FeatherIcons.fileText
                            |> FeatherIcons.toHtml []
                        ]
                    ]
                , noteEditor content
                ]

        Viewing id content ->
            div []
                [ row ""
                    []
                    [ spacer
                    , button [ onClick <| StartEditing id ]
                        [ FeatherIcons.edit3
                            |> FeatherIcons.toHtml []
                        ]
                    ]
                , div [ class " pv2 " ] <| Markdown.toHtml Nothing content
                ]

        NotFound id ->
            div [] [ text "Note Not Found" ]


noteEditor content =
    div [ class "flex-auto flex" ]
        [ textarea
            [ class "pa2 min-h-100 w-100"
            , value content
            , onInput ContentChanged
            , onBlur ContentBlurred
            ]
            []
        ]
