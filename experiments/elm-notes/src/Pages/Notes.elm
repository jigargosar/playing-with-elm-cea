module Pages.Notes exposing (..)

import Collection
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Note exposing (Note)
import Session exposing (Session)
import Skeleton


type alias Model =
    { session : Session }


type Msg
    = Nop


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


update message model =
    case message of
        Nop ->
            ( model, Cmd.none )


currentNoteList : Model -> List Note
currentNoteList =
    .session
        >> .nc
        >> Collection.items
        >> List.filter (.deleted >> not)
        >> List.sortBy (.modifiedAt >> (*) -1)


view : Model -> Skeleton.Details Msg
view model =
    { title = "Notes"
    , attrs = []
    , kids =
        [ bbtn Nop "New", viewNotes (currentNoteList model) ]
    }


viewNotes notes =
    let
        viewItem note =
            div [ class "bb b--black-10 pv2" ]
                [ viewNoteItem note
                ]
    in
        div [ class "pv1 vs1" ] <| List.map viewItem notes


viewNoteItem note =
    let
        lines =
            Note.getContent note |> String.trim |> String.lines

        firstLine =
            List.head lines |> Maybe.withDefault "<Empty>"

        otherLines =
            List.tail lines |> Maybe.withDefault [] |> String.join " " |> String.slice 0 100

        --            routeToNoteDetailViewMsg =
        --                RouteTo <| NoteDetail note.id
    in
        div
            [ {- onClick routeToNoteDetailViewMsg
                 , Exts.Html.Events.onEnter routeToNoteDetailViewMsg
                 ,
              -}
              class "link black pv2 pointer "
            , tabindex 0
            ]
            [ div [ class "f5" ] [ text firstLine ]
            , div [ class "f6 truncate black-60" ] [ text otherLines ]
            ]


bbtn msg title =
    button [ class "ttu", onClick msg ] [ text title ]
