module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text, textarea)
import Html.Attributes exposing (attribute, autofocus, class, src, value)
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
import NoteCollection exposing (NoteCollection)
import Random


---- MODEL ----


type EditState
    = NotEditing
    | EditingNew String
    | Editing Note String


type alias Flags =
    { now : Int }


type alias Model =
    { noteCollection : NoteCollection, editState : EditState }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( noteCollection, _ ) =
            Random.step NoteCollection.generator (Random.initialSeed flags.now)
    in
        ( { noteCollection = noteCollection
          , editState = NotEditing
          }
        , Cmd.none
        )


currentNoteList : Model -> List Note
currentNoteList =
    .noteCollection >> NoteCollection.all



---- UPDATE ----


type EditMsg
    = OnNew
    | OnEdit Note
    | OnOk
    | OnCancel
    | OnUpdate String


type Msg
    = NoOp
    | EditMsg EditMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditMsg editMsg ->
            case ( model.editState, editMsg ) of
                ( _, OnNew ) ->
                    ( { model | editState = EditingNew "" }, Cmd.none )

                ( _, OnEdit note ) ->
                    ( { model | editState = Editing note note.content }, Cmd.none )

                ( EditingNew content, OnUpdate updatedContent ) ->
                    ( { model | editState = EditingNew updatedContent }, Cmd.none )

                ( EditingNew content, OnOk ) ->
                    ( { model
                        | editState = NotEditing
                        , noteCollection = NoteCollection.addNew content model.noteCollection
                      }
                    , Cmd.none
                    )

                ( Editing note content, OnUpdate updatedContent ) ->
                    ( { model | editState = Editing note updatedContent }, Cmd.none )

                ( Editing note content, OnOk ) ->
                    ( { model
                        | editState = NotEditing
                        , noteCollection = NoteCollection.updateNoteContent content note model.noteCollection
                      }
                    , Cmd.none
                    )

                ( _, OnCancel ) ->
                    ( { model | editState = NotEditing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "pv3 flex flex-column vh-100 vs3" ]
        [ div [ class "center w-90" ]
            [ div [ class "f3 tc" ] [ H.text "My Elm App" ]
            , viewAddNewNote model.editState
            ]
        , div [ class "flex-auto overflow-scroll" ]
            [ div [ class "center w-90" ]
                [ viewNoteList model.editState (currentNoteList model)
                ]
            ]
        ]


bbtn msg title =
    button [ class "ttu", onClick msg ] [ text title ]


viewAddNewNote editState =
    div [ class "vs3" ]
        (case editState of
            EditingNew content ->
                [ div []
                    [ textarea
                        [ class "w-100 h5"
                        , autofocus True
                        , value content
                        , onInput OnUpdate
                        ]
                        []
                    ]
                , div [ class "flex hs3" ]
                    [ bbtn OnOk "Ok"
                    , bbtn OnCancel "Cancel"
                    ]
                ]

            _ ->
                [ bbtn OnNew "New"
                ]
        )
        |> Html.map EditMsg


isEditingNote note editState =
    case editState of
        Editing editingNote content ->
            note == editingNote

        _ ->
            False


viewNoteList editState notes =
    let
        viewNoteListItem note =
            div [ class "vs3" ]
                (case ( editState, isEditingNote note editState ) of
                    ( Editing eNote content, True ) ->
                        [ div []
                            [ textarea
                                [ class "w-100 h5"
                                , autofocus True
                                , value content
                                , onInput OnUpdate
                                ]
                                []
                            ]
                        , div [ class "flex hs3" ]
                            [ bbtn OnOk "Ok"
                            , bbtn OnCancel "Cancel"
                            ]
                        ]

                    _ ->
                        [ div
                            [ attribute "role" "button"
                            , onClick <| OnEdit note
                            ]
                            [ text <| Note.title note ]
                        ]
                )
                |> Html.map EditMsg
    in
        div [ class "vs3" ] <| List.map viewNoteListItem notes



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
