port module Main exposing (..)

import Browser
import Exts.Html.Events
import Exts.List
import Exts.Maybe exposing (maybe)
import HotKey exposing (defaultHotKey)
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
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Keyboard.Event
import Keyboard.Key
import Markdown
import Note exposing (Note)
import NoteCollection exposing (NoteCollection)
import Random
import Task
import Time
import Update.Extra


port persistNoteCollection : E.Value -> Cmd msg


port sessionChanged : (E.Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port notesCollectionChanged : (E.Value -> msg) -> Sub msg



---- MODEL ----


type alias User =
    { uid : String
    , email : String
    , displayName : String
    }


type Session
    = Auth User
    | Anon
    | InitialUnknown


type EditState
    = NotEditing
    | EditingNew String
    | Editing Note String


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { noteCollection : NoteCollection
    , editState : EditState
    , lastFocusedNoteListItemDomId : String
    , session : Session
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( noteCollection, _ ) =
            Random.step (NoteCollection.generator flags.noteList) (Random.initialSeed flags.now)
    in
        ( { noteCollection = noteCollection
          , editState = NotEditing
          , lastFocusedNoteListItemDomId = ""
          , session = InitialUnknown
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
    | OnDelete
    | OnUpdate NoteContent
    | EditMsgNoOp


type Msg
    = NoOp
    | EditMsg EditMsg
    | SetNoteCollection NoteCollection
    | AddNote NoteContent Millis
    | DeleteNote Note
    | SetLastFocusedNoteListItemDomId String
    | SetNotEditing
    | Session E.Value
    | SignIn
    | SignOut
    | NotesCollectionChanged E.Value


type alias UpdateReturn msg model =
    ( model, Cmd msg )


type alias F a =
    a -> a


addCmd : Cmd msg -> F (UpdateReturn msg model)
addCmd cmd =
    addEffect (always cmd)


addEffect fn ( model, oldCmd ) =
    ( model, Cmd.batch [ oldCmd, fn model ] )


andThen _ =
    identity


sequence =
    Update.Extra.sequence update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NotesCollectionChanged encNC ->
            let
                newNC : NoteCollection
                newNC =
                    NoteCollection.replace encNC model.noteCollection
            in
                update (SetNoteCollection newNC) model

        SignIn ->
            ( model, signIn () )

        SignOut ->
            ( model, signOut () )

        Session encSession ->
            let
                userDecoder : Decoder User
                userDecoder =
                    D.map3 User
                        (D.field "uid" D.string)
                        (D.field "email" D.string)
                        (D.field "displayName" D.string)

                sessionDecoder : Decoder Session
                sessionDecoder =
                    D.oneOf [ D.null Anon, D.map Auth userDecoder ]

                newSession =
                    D.decodeValue sessionDecoder encSession
                        --                        |> Result.mapError (Debug.log "Error: session")
                        |> Result.withDefault model.session
            in
                ( { model | session = newSession }, Cmd.none )

        SetLastFocusedNoteListItemDomId domId ->
            ( if isNoteListItemDomId domId then
                { model | lastFocusedNoteListItemDomId = domId }
              else
                model
            , Cmd.none
            )

        SetNoteCollection nc ->
            ( { model | noteCollection = nc }, persistNoteCollection <| NoteCollection.encode nc )

        DeleteNote note ->
            let
                nowMillisT =
                    Time.now
                        |> Task.map Time.posixToMillis
            in
                ( model
                  --            , nowMillis
                  --                (\now ->
                  --                    SetNoteCollection <| NoteCollection.delete now note model.noteCollection
                  --                )
                , nowMillisT
                    |> Task.map (\now -> NoteCollection.delete now note model.noteCollection)
                    |> Task.perform SetNoteCollection
                )

        AddNote content now ->
            let
                ( note, nc ) =
                    NoteCollection.addNew now content model.noteCollection
            in
                update (SetNoteCollection nc) model
                    |> addCmd (focusNoteListItem note)

        SetNotEditing ->
            ( { model | editState = NotEditing }, Cmd.none )
                |> addEffect restoreNoteListItemFocus

        EditMsg editMsg ->
            case ( model.editState, editMsg ) of
                ( _, OnNew ) ->
                    ( { model | editState = EditingNew "" }, focusEditor )

                ( _, OnEdit note ) ->
                    ( { model | editState = Editing note note.content }, focusEditor )

                ( EditingNew content, OnUpdate updatedContent ) ->
                    ( { model | editState = EditingNew updatedContent }, Cmd.none )

                ( EditingNew content, OnOk ) ->
                    update SetNotEditing model
                        |> addCmd (nowMillis <| AddNote content)

                ( Editing note content, OnUpdate updatedContent ) ->
                    ( { model | editState = Editing note updatedContent }, Cmd.none )

                ( Editing note content, OnDelete ) ->
                    ( model, Cmd.none ) |> sequence [ DeleteNote note, SetNotEditing ]

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
                    update SetNotEditing model

                _ ->
                    ( model, Cmd.none )


nowMillis msg =
    Task.perform (Time.posixToMillis >> msg) Time.now


focus id =
    Task.attempt
        (\r ->
            --            let
            --                _ =
            --                    r |> Result.mapError (Debug.log "Error")
            --            in
            NoOp
        )
        (B.focus id)


focusEditor =
    focus "editor"


focusNoteListItem =
    noteListItemDomId >> focus


restoreNoteListItemFocus =
    .lastFocusedNoteListItemDomId >> focus



---- VIEW ----


decodeTargetId =
    D.at [ "target", "id" ] D.string


onFocusInTargetId : (String -> msg) -> Html.Attribute msg
onFocusInTargetId msg =
    Html.Events.on "focusin" (decodeTargetId |> D.map msg)


view : Model -> Html Msg
view model =
    div [ class "pv3 flex flex-column vh-100 vs3", onFocusInTargetId SetLastFocusedNoteListItemDomId ]
        [ div [ class "vs3 center w-90" ]
            [ viewHeader model.session
            , viewAddNewNote model.editState
            ]
        , div [ class "flex-auto overflow-scroll" ]
            [ div [ class "center w-90" ]
                [ viewNoteList model.editState (currentNoteList model)
                ]
            ]
        ]


viewHeader session =
    let
        viewSession =
            div [ class "flex items-center hs3" ]
                (case session of
                    InitialUnknown ->
                        [ div [] [ text <| "InitialUnknown" ] ]

                    Anon ->
                        [ div [] [ text <| "SignedOut" ], bbtn SignIn "SignIn" ]

                    Auth user ->
                        [ div [] [ text <| "SignedIn: " ++ user.displayName ], bbtn SignOut "SignOut" ]
                )
    in
        div [ class "flex items-center hs3" ]
            [ div [ class "f3 tc" ] [ H.text "Elm Notes" ]
            , viewSession
            ]


bbtn msg title =
    button [ class "ttu", onClick msg ] [ text title ]


viewAddNewNote editState =
    div [ class "vs3" ]
        (case editState of
            EditingNew content ->
                [ noteContentEditor content
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


noteListItemDomIdPrefix =
    "note-li-"


noteListItemDomId note =
    noteListItemDomIdPrefix ++ (Note.idStr note)


isNoteListItemDomId =
    String.startsWith noteListItemDomIdPrefix


noteContentEditor content =
    let
        mapping =
            [ ( HotKey.esc, OnCancel )
            , ( HotKey.metaEnter, OnOk )
            ]
    in
        div []
            [ textarea
                [ id "editor"
                , class "w-100 h5"
                , autofocus True
                , value content
                , onInput OnUpdate
                , HotKey.onKeyDown mapping EditMsgNoOp
                ]
                []
            ]


viewNoteListEditItem content =
    (div [ class "vs2" ]
        [ noteContentEditor content
        , div [ class "flex hs3" ]
            [ bbtn OnOk "Ok"
            , bbtn OnCancel "Cancel"
            , bbtn OnDelete "Delete"
            ]
        ]
    )
        |> Html.map EditMsg


viewNoteListDisplayItem note =
    let
        nodeDomId =
            noteListItemDomId note

        content =
            Note.getContent note

        lines =
            String.trim content |> String.lines

        firstLine =
            List.head lines |> Maybe.withDefault "<Empty>"

        otherLines =
            List.tail lines |> Maybe.withDefault [] |> String.join " " |> String.slice 0 100

        startEditingMsg =
            EditMsg <| OnEdit note
    in
        div
            [ id nodeDomId
            , onFocus (SetLastFocusedNoteListItemDomId nodeDomId)
            , onClick startEditingMsg
            , Exts.Html.Events.onEnter startEditingMsg
            , class " pv2 pointer "
            , tabindex 0
            ]
            [ div [ class "f5" ] [ text firstLine ]
            , div [ class "f6 truncate black-60" ] [ text otherLines ]

            {- , div [] <| Markdown.toHtml Nothing content -}
            ]


viewNoteList editState notes =
    let
        viewItem note =
            div [ class "bb b--black-10 pv2" ]
                [ case ( editState, isEditingNote note editState ) of
                    ( Editing _ content, True ) ->
                        viewNoteListEditItem content

                    _ ->
                        viewNoteListDisplayItem note
                ]
    in
        div [ class "pv1 vs1" ] <| List.map viewItem notes



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ sessionChanged Session
        , notesCollectionChanged NotesCollectionChanged
        ]


main : Program Flags Model Msg
main =
    B.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
