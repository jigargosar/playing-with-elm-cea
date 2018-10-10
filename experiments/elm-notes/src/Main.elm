port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text, textarea)
import Html.Attributes exposing (attribute, autofocus, class, src, tabindex, value)
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
import Task
import Time


port persistNoteCollection : E.Value -> Cmd msg



---- MODEL ----


type EditState
    = NotEditing
    | EditingNew String
    | Editing Note String


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { noteCollection : NoteCollection, editState : EditState }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( noteCollection, _ ) =
            Random.step (NoteCollection.generator flags.now flags.noteList) (Random.initialSeed flags.now)
    in
        ( { noteCollection = noteCollection
          , editState = NotEditing
          }
        , Cmd.none
        )


currentNoteList : Model -> List Note
currentNoteList =
    .noteCollection >> NoteCollection.queryAll



---- UPDATE ----


type alias Millis =
    Int


type alias NoteContent =
    String


type EditMsg
    = OnNew
    | OnEdit Note
    | OnOk
    | OnCancel
    | OnUpdate NoteContent


type Msg
    = NoOp
    | EditMsg EditMsg
    | SetNoteCollection NoteCollection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetNoteCollection nc ->
            ( { model | noteCollection = nc }, persistNoteCollection <| NoteCollection.encode nc )

        EditMsg editMsg ->
            case ( model.editState, editMsg ) of
                ( _, OnNew ) ->
                    ( { model | editState = EditingNew "" }, Cmd.none )

                ( _, OnEdit note ) ->
                    ( { model | editState = Editing note note.content }, Cmd.none )

                ( EditingNew content, OnUpdate updatedContent ) ->
                    ( { model | editState = EditingNew updatedContent }, Cmd.none )

                ( EditingNew content, OnOk ) ->
                    ( { model | editState = NotEditing }
                    , nowMillis
                        (\now ->
                            NoteCollection.addNew now content model.noteCollection
                                |> SetNoteCollection
                        )
                    )

                ( Editing note content, OnUpdate updatedContent ) ->
                    ( { model | editState = Editing note updatedContent }, Cmd.none )

                ( Editing note content, OnOk ) ->
                    ( { model | editState = NotEditing }
                    , nowMillis
                        (\now ->
                            NoteCollection.updateNoteContent now content note model.noteCollection
                                |> SetNoteCollection
                        )
                    )

                ( _, OnCancel ) ->
                    ( { model | editState = NotEditing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


nowMillis msg =
    Task.perform (Time.posixToMillis >> msg) Time.now



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
            (case ( editState, isEditingNote note editState ) of
                ( Editing eNote content, True ) ->
                    div [ class "vs2 pv2" ]
                        [ div []
                            [ textarea
                                [ class "w-100 h4"
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
                    button
                        [ onClick <| OnEdit note
                        , class "input-reset tl bn bg--transparent db pv2 ph2 w-100 pointer "
                        ]
                        [ text <| Note.title note ]
            )
                |> Html.map EditMsg
    in
        div [ class "pv1" ] <| List.map viewNoteListItem notes



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
