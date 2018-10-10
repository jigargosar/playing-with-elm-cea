module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Browser as B
import Browser.Events as B
import Browser.Events as BE
import Browser.Dom as B
import Browser.Dom as BD
import Html as H exposing (Html)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Json.Decode as D
import Json.Encode as E
import Note exposing (Note)


---- MODEL ----


type alias Flags =
    { now : Int }


type alias Model =
    { notes : List Note }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { notes = [ "Note 1", "Project Notes Brainstorming" ] |> List.map Note.init
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "pa2 center measure" ]
        [ div [ class "flex flex-column vs3 " ]
            [ div [ class "f3 tc" ] [ H.text "My Elm App" ]
            , div [] [ viewAddNote ]
            , div [] [ viewNoteList model.notes ]
            ]
        ]


viewAddNote =
    button [] [ text "New" ]


viewNoteList notes =
    let
        viewNoteListItem note =
            div [] [ text (Note.title note) ]
    in
        div [] (List.map viewNoteListItem notes)



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch []


main : Program Flags Model Msg
main =
    B.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
