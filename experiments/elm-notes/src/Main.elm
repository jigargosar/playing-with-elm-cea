module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text, textarea)
import Html.Attributes exposing (class, src)
import Browser as B
import Browser.Events as B
import Browser.Events as BE
import Browser.Dom as B
import Browser.Dom as BD
import Html as H exposing (Html)
import Html.Events exposing (onClick)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Json.Decode as D
import Json.Encode as E
import Note exposing (Note)


---- MODEL ----


type Edit
    = Closed
    | New String


type alias Flags =
    { now : Int }


type alias Model =
    { notes : List Note, edit : Edit }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { notes = [ "Note 1", "Project Notes Brainstorming" ] |> List.map Note.init
      , edit = Closed
      }
    , Cmd.none
    )



---- UPDATE ----


type EditMsg
    = OnNew
    | OnOk
    | OnCancel


type Msg
    = NoOp
    | Edit EditMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Edit editMsg ->
            case ( model.edit, editMsg ) of
                ( _, OnNew ) ->
                    ( { model | edit = New "" }, Cmd.none )

                ( New content, OnOk ) ->
                    ( { model | edit = Closed, notes = Note.init content :: model.notes }, Cmd.none )

                ( _, OnCancel ) ->
                    ( { model | edit = Closed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "pa2 center measure" ]
        [ div [ class "vs3 " ]
            [ div [ class "f3 tc" ] [ H.text "My Elm App" ]
            , div [] [ viewAddNewNote ]
            , div [] [ viewNoteList model.notes ]
            ]
        ]


bbtn msg title =
    button [ onClick msg ] [ text title ]


viewAddNewNote =
    div [ class "vs3" ]
        [ bbtn (Edit OnNew) "New"
        , div [] [ textarea [] [] ]
        , bbtn (Edit OnOk) "ok"
        , bbtn (Edit OnCancel) "cancel"
        ]


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
