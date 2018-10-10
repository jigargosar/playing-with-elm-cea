module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text, textarea)
import Html.Attributes exposing (class, src, value)
import Browser as B
import Browser.Events as B
import Browser.Events as BE
import Browser.Dom as B
import Browser.Dom as BD
import Html as H exposing (Html)
import Html.Events exposing (onClick, onInput)
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
    | OnUpdate String


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

                ( New content, OnUpdate updatedContent ) ->
                    ( { model | edit = New updatedContent }, Cmd.none )

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
            , div [] [ viewAddNewNote model.edit ]
            , div [] [ viewNoteList model.notes ]
            ]
        ]


bbtn msg title =
    button [ class "ttu", onClick msg ] [ text title ]


viewAddNewNote edit =
    case edit of
        New content ->
            div [ class "vs3" ]
                [ div [] [ textarea [ class "w-100 h5", value content, onInput (Edit << OnUpdate) ] [] ]
                , div [ class "flex hs3" ] [ bbtn (Edit OnOk) "Ok", bbtn (Edit OnCancel) "Cancel" ]
                ]

        _ ->
            div [ class "vs3" ]
                [ bbtn (Edit OnNew) "New"
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
