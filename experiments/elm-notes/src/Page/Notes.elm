module Page.Notes exposing (Model, Msg, init, subscriptions, update, view)

import Collection
import Exts.Html.Events
import FeatherIcons
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Note exposing (Note)
import NotesCollection exposing (NotesCollection)
import Ports
import Session exposing (Session)
import Set exposing (Set)
import Skeleton exposing (defaultSkeletonDetails)
import Task
import UI exposing (link)


type alias Model =
    { session : Session, selection : Set Note.Id }


type Msg
    = Nop
    | ViewNote Note
    | New
    | Delete
    | SelectionDeleted ( NotesCollection, Cmd Msg )
    | NCC E.Value
    | Toggle Note
    | Clear
    | All


subscriptions =
    Sub.batch [ Ports.notesCollectionChanged NCC ]


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, selection = Set.empty }, Cmd.none )


getNC =
    .session >> .nc


overSession : (Session -> Session) -> Model -> Model
overSession updateFn model =
    { model | session = updateFn model.session }


toggleMember item set =
    if Set.member item set then
        Set.remove item set

    else
        Set.insert item set


isSelected note =
    .selection >> Set.member note.id


overSelection : (Set Note.Id -> Set Note.Id) -> Model -> Model
overSelection updateFn model =
    { model | selection = updateFn model.selection }


clearSelection =
    overSelection (always Set.empty)


update message model =
    case message of
        Nop ->
            ( model, Cmd.none )

        Toggle note ->
            ( overSelection (toggleMember note.id) model, Cmd.none )

        Clear ->
            ( clearSelection model, Cmd.none )

        All ->
            ( overSelection (always (getNC model |> Collection.idList |> Set.fromList)) model, Cmd.none )

        ViewNote note ->
            ( model, Session.pushHref (Href.viewNoteId note.id) model.session )

        SelectionDeleted ( nc, cmd ) ->
            ( model
                |> overSession (Session.setNC nc)
                |> clearSelection
            , cmd
            )

        New ->
            ( model, Cmd.none )

        Delete ->
            ( model
            , NotesCollection.deleteAllWithIds model.selection (getNC model)
                |> Task.perform SelectionDeleted
            )

        NCC encVal ->
            let
                ( nc, cmd ) =
                    NotesCollection.replace encVal (getNC model)
            in
            ( model |> overSession (Session.setNC nc), cmd )


currentNoteList : Model -> List Note
currentNoteList =
    getNC
        >> Collection.items
        >> List.filter (.deleted >> not)
        >> List.sortBy (.modifiedAt >> (*) -1)


view : Model -> Skeleton.Details Msg
view model =
    { defaultSkeletonDetails
        | title = "Notes"
        , kids = viewKids model
        , mm = { actions = getMMActions model }
    }


getMMActions model =
    [ { icon = FeatherIcons.filePlus, msg = New } ]


viewKids model =
    let
        hasSelection =
            Set.isEmpty model.selection |> not

        viewItem note =
            let
                selected =
                    isSelected note model
            in
            div
                [ class "flex flex-row items-center hs3 bb b--black-10"
                , classList [ ( "bg-light-yellow", selected ) ]
                ]
                [ button [ onClick <| Toggle note ]
                    [ (if selected then
                        FeatherIcons.check

                       else
                        FeatherIcons.minus
                      )
                        |> FeatherIcons.toHtml []
                    ]
                , viewNoteContent hasSelection selected note
                , if hasSelection then
                    text ""

                  else
                    a [ href (Href.editNoteId note.id) ] [ button [] [ FeatherIcons.edit3 |> FeatherIcons.toHtml [] ] ]
                ]

        viewNotes =
            div [ class "pv3" ] <| List.map viewItem (currentNoteList model)
    in
    [ div [ class "flex items-center hs3" ]
        (if hasSelection then
            [ button [ onClick All ]
                [ FeatherIcons.checkSquare
                    |> FeatherIcons.toHtml []
                ]
            , button [ onClick Clear ]
                [ FeatherIcons.slash
                    |> FeatherIcons.toHtml []
                ]
            , div [ class "flex-auto" ] []
            , button [ onClick Delete ]
                [ FeatherIcons.trash2
                    |> FeatherIcons.toHtml []
                ]
            ]

         else
            [ div [ class "flex-auto" ] []
            , a [ href Href.newNote ] [ button [] [ FeatherIcons.filePlus |> FeatherIcons.toHtml [] ] ]
            ]
        )
    , viewNotes
    ]


viewNoteContent hasSelection selected note =
    let
        lines =
            Note.getContent note |> String.trim |> String.lines

        firstLine =
            List.head lines |> Maybe.withDefault "<Empty>"

        otherLines =
            List.tail lines |> Maybe.withDefault [] |> String.join " " |> String.slice 0 100

        onClickMsg =
            if hasSelection then
                Toggle note

            else
                ViewNote note
    in
    div
        [ onClick onClickMsg
        , Exts.Html.Events.onEnter onClickMsg
        , class "flex-auto black pv3 pointer "

        --            , href ("note/" ++ note.id)
        , tabindex 0
        ]
        [ div [ class "f5" ] [ text firstLine ]
        , div [ class "f6 truncate black-60" ] [ text otherLines ]
        ]
