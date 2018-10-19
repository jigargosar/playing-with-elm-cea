module Pages.Notes exposing (..)

import Collection
import Exts.Html.Events
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note exposing (Note)
import Ports
import Session exposing (Session)
import Skeleton
import Task
import Json.Decode as D
import Json.Encode as E
import NotesCollection
import Set exposing (Set)
import UI exposing (link)


type alias Model =
    { session : Session, selection : Set Note.Id }


type Msg
    = Nop
    | ViewNote Note
    | NCC E.Value
    | ToggleSelection Note


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


update message model =
    case message of
        Nop ->
            ( model, Cmd.none )

        ToggleSelection note ->
            ( { model | selection = toggleMember note.id model.selection }, Cmd.none )

        ViewNote note ->
            ( model, Session.pushHref (Href.viewNoteId note.id) model.session )

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
    { title = "Notes"
    , attrs = []
    , kids =
        [ a [ class "link", href Href.newNote ] [ text "New" ], viewNotes (currentNoteList model) ]
    }


viewNotes notes =
    let
        viewItem note =
            div [ class "flex flex-row items-center hs3" ]
                [ div [ onClick <| ToggleSelection note ] [ text "--" ]
                , viewNoteItem note
                , link "/" "e"
                ]
    in
        div [ class "pv3" ] <| List.map viewItem notes


viewNoteItem note =
    let
        lines =
            Note.getContent note |> String.trim |> String.lines

        firstLine =
            List.head lines |> Maybe.withDefault "<Empty>"

        otherLines =
            List.tail lines |> Maybe.withDefault [] |> String.join " " |> String.slice 0 100

        viewNoteMsg =
            ViewNote note
    in
        div
            [ onClick viewNoteMsg
            , Exts.Html.Events.onEnter viewNoteMsg
            , class "flex-auto link black pv3 pointer bb b--black-10"
            , href ("note/" ++ note.id)
            , tabindex 0
            ]
            [ div [ class "f5" ] [ text firstLine ]
            , div [ class "f6 truncate black-60" ] [ text otherLines ]
            ]
