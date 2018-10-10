port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text, textarea)
import Html.Attributes exposing (attribute, autofocus, class, id, src, tabindex, value)
import Browser as B
import Browser.Events as B
import Browser.Events as BE
import Browser.Dom as B
import Browser.Dom as BD
import Html as H exposing (Html)
import Html.Events exposing (onClick, onFocus, onInput)
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
    { noteCollection : NoteCollection
    , editState : EditState
    , lastFocusedDomId : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( noteCollection, _ ) =
            Random.step (NoteCollection.generator flags.now flags.noteList) (Random.initialSeed flags.now)
    in
        ( { noteCollection = noteCollection
          , editState = NotEditing
          , lastFocusedDomId = ""
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
    | AddNote NoteContent Millis
    | PushLastFocusedDomId String


addCmd cmd =
    let
        batch2 oldCmd =
            Cmd.batch [ oldCmd, cmd ]
    in
        Tuple.mapSecond batch2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PushLastFocusedDomId domId ->
            let
                _ =
                    Debug.log "SetLastFocusedDomId" domId
            in
                ( if String.isEmpty domId then
                    model
                  else
                    { model | lastFocusedDomId = domId }
                , Cmd.none
                )

        SetNoteCollection nc ->
            ( { model | noteCollection = nc }, persistNoteCollection <| NoteCollection.encode nc )

        AddNote content now ->
            let
                ( note, nc ) =
                    NoteCollection.addNew now content model.noteCollection
            in
                update (SetNoteCollection nc) model
                    |> addCmd (focusNoteListItem note)

        EditMsg editMsg ->
            case ( model.editState, editMsg ) of
                ( _, OnNew ) ->
                    ( { model | editState = EditingNew "" }, focusEditor )

                ( _, OnEdit note ) ->
                    ( { model | editState = Editing note note.content }, focusEditor )

                ( EditingNew content, OnUpdate updatedContent ) ->
                    ( { model | editState = EditingNew updatedContent }, Cmd.none )

                ( EditingNew content, OnOk ) ->
                    ( { model | editState = NotEditing }
                    , nowMillis <| AddNote content
                    )

                ( Editing note content, OnUpdate updatedContent ) ->
                    ( { model | editState = Editing note updatedContent }, Cmd.none )

                ( Editing note content, OnOk ) ->
                    ( { model | editState = NotEditing }
                    , Cmd.batch
                        [ nowMillis
                            (\now ->
                                NoteCollection.updateNoteContent now content note model.noteCollection
                                    |> SetNoteCollection
                            )
                        , focusNoteListItem note
                        ]
                    )

                ( _, OnCancel ) ->
                    ( { model | editState = NotEditing }, focus model.lastFocusedDomId )

                _ ->
                    ( model, Cmd.none )


nowMillis msg =
    Task.perform (Time.posixToMillis >> msg) Time.now


focus id =
    Task.attempt
        (\r ->
            let
                _ =
                    r |> Result.mapError (Debug.log "Error")
            in
                NoOp
        )
        (B.focus id)


focusEditor =
    focus "editor"


focusNoteListItem =
    noteListItemDomId >> focus



---- VIEW ----


onFocusInTargetId : (String -> msg) -> Html.Attribute msg
onFocusInTargetId msg =
    Html.Events.on "focusin" (D.at [ "target", "id" ] D.string |> D.map msg)


view : Model -> Html Msg
view model =
    div [ class "pv3 flex flex-column vh-100 vs3", onFocusInTargetId PushLastFocusedDomId ]
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
                        [ id "editor"
                        , class "w-100 h5"
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


noteListItemDomId note =
    "note-li-" ++ note.id


viewNoteList editState notes =
    let
        viewNoteListItem note =
            (case ( editState, isEditingNote note editState ) of
                ( Editing eNote content, True ) ->
                    div [ class "vs2 pv2" ]
                        [ div []
                            [ textarea
                                [ id "editor"
                                , class "w-100 h4"
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
                        |> Html.map EditMsg

                _ ->
                    let
                        nodeDomId =
                            noteListItemDomId note
                    in
                        button
                            [ id nodeDomId
                            , onFocus (PushLastFocusedDomId nodeDomId)
                            , onClick <| EditMsg <| OnEdit note
                            , class "input-reset tl bn bg--transparent db pv2 ph2 w-100 pointer "
                            ]
                            [ text <| Note.title note ]
            )
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
