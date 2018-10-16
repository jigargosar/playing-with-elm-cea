port module Main exposing (..)

import Browser
import Editable
import Exts.Html.Events
import Exts.List
import Exts.Maybe exposing (maybe)
import HotKey exposing (defaultHotKey)
import Html exposing (Html, button, div, h1, img, text, textarea)
import Html.Attributes exposing (attribute, autofocus, class, href, id, src, tabindex, value)
import Browser as B
import Browser.Navigation as Nav
import Browser.Events as B
import Browser.Events as BE
import Browser.Dom as B
import Browser.Dom as BD
import Html as H exposing (Html)
import Html.Events exposing (onClick, onFocus, onInput)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Id exposing (Id)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Keyboard.Event
import Keyboard.Key
import Markdown
import Note exposing (Note)
import NoteCollection exposing (NoteCollection)
import Pages.EditNote as EditNote
import Random
import Task
import Time
import Update.Extra
import Url
import Url.Builder
import Url.Parser as UrlPar exposing ((</>))
import Return2 as R2
import Return3 as R3
import UserInput


port persistNoteCollection : E.Value -> Cmd msg


port sessionChanged : (E.Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port notesCollectionChanged : (E.Value -> msg) -> Sub msg



---- MODEL ----


type Route
    = NoteList
    | NoteDetail Id
    | NoteEdit Id
    | NotFound Url.Url


routeParser : UrlPar.Parser (Route -> a) a
routeParser =
    UrlPar.oneOf
        [ UrlPar.map NoteList UrlPar.top
        , UrlPar.map (Id.fromString >> NoteDetail) (UrlPar.s "note" </> UrlPar.string)
        , UrlPar.map (Id.fromString >> NoteEdit) (UrlPar.s "note" </> UrlPar.s "edit" </> UrlPar.string)
        ]


routeToUrlString route =
    let
        absolute =
            Url.Builder.absolute
    in
        case route of
            NoteList ->
                absolute [] []

            NoteDetail id ->
                absolute [ "note", Id.toString id ] []

            NoteEdit id ->
                absolute [ "note", "edit", Id.toString id ] []

            NotFound url ->
                Url.toString url


routeFromUrl url =
    UrlPar.parse routeParser url |> Maybe.withDefault (NotFound url)


pageFromUrl url hasNC =
    let
        route =
            routeFromUrl url
    in
        case route of
            NoteEdit id ->
                (getNoteById id hasNC)
                    |> Maybe.map createNoteEditPage
                    |> Maybe.withDefault (NotFoundPage "Note Not Found")

            NoteDetail id ->
                (getNoteById id hasNC)
                    |> Maybe.map (\n -> NoteDetailPage n)
                    |> Maybe.withDefault (NotFoundPage "Note Not Found")

            NoteList ->
                NoteListPage

            NotFound _ ->
                NotFoundPage "Oops Something went wrong"


type alias User =
    { uid : String
    , email : String
    , displayName : String
    }


type Session
    = Auth User
    | Anon
    | InitialUnknown


type Page
    = EditNotePage EditNote.Model
    | NotFoundPage String
    | NoteListPage
    | NoteDetailPage Note


createNoteEditPage note =
    EditNotePage <| EditNote.init note


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { noteCollection : NoteCollection
    , lastFocusedNoteListItemDomId : String
    , session : Session
    , key : Nav.Key
    , url : Url.Url
    , page : Page
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        _ =
            Browser.application

        ( noteCollection, _ ) =
            Random.step (NoteCollection.generator flags.noteList) (Random.initialSeed flags.now)
    in
        ( { noteCollection = noteCollection
          , lastFocusedNoteListItemDomId = ""
          , session = InitialUnknown
          , key = key
          , url = url
          , page = pageFromUrl url { noteCollection = noteCollection }
          }
        , Cmd.none
        )


currentNoteList : Model -> List Note
currentNoteList =
    .noteCollection >> NoteCollection.queryAll


getNoteById id =
    .noteCollection >> NoteCollection.getById id



---- UPDATE ----


type alias Millis =
    Int


type alias NoteContent =
    String


type Msg
    = NoOp
    | SetNoteCollection NoteCollection
    | SetNoteContent String Note Int
    | AddNote NoteContent Millis
    | DeleteNote Note
    | Session E.Value
    | SignIn
    | SignOut
    | NotesCollectionChanged E.Value
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | RouteTo Route
    | PushIfChanged String
    | EditNotePageMsg EditNote.Msg


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ sessionChanged Session
        , notesCollectionChanged NotesCollectionChanged
        ]


type alias UpdateReturn msg model =
    ( model, Cmd msg )


type alias F a =
    a -> a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PushIfChanged urlStr ->
            let
                currentUrlStr =
                    Url.toString model.url
            in
                if urlStr /= currentUrlStr then
                    ( model, Nav.pushUrl model.key urlStr )
                else
                    ( model, Cmd.none )

        RouteTo route ->
            update (PushIfChanged <| routeToUrlString route) model

        LinkClicked urlReq ->
            case urlReq of
                Browser.Internal url ->
                    update (PushIfChanged <| Url.toString url) model

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, page = pageFromUrl url model }
            , Cmd.none
            )

        EditNotePageMsg pageMsg ->
            case model.page of
                EditNotePage pageModel ->
                    pageModel
                        |> EditNote.update pageMsg
                        |> R3.mapCmd EditNotePageMsg
                        |> R3.incorp handleEditNotePageReply model

                _ ->
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

        SetNoteCollection nc ->
            ( { model | noteCollection = nc }, persistNoteCollection <| NoteCollection.encode nc )

        SetNoteContent content note now ->
            update
                (SetNoteCollection <| NoteCollection.updateNoteContent now content note model.noteCollection)
                model

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


withNowMillis msg =
    Task.perform (Time.posixToMillis >> msg) Time.now


handleEditNotePageReply pageModel maybeReply model =
    case maybeReply of
        Just (EditNote.SaveContent note content) ->
            ( { model | page = EditNotePage pageModel }
            , withNowMillis (SetNoteContent content note)
            )

        Nothing ->
            ( { model | page = EditNotePage pageModel }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Notes", body = [ htmlView model ] }


htmlView : Model -> Html Msg
htmlView model =
    case model.page of
        EditNotePage pageRec ->
            viewNoteEditPage model pageRec

        NoteDetailPage note ->
            viewNoteDetailPage model note

        NotFoundPage message ->
            viewNotFoundPage model message

        NoteListPage ->
            viewNoteListPage model


bbtn msg title =
    button [ class "ttu", onClick msg ] [ text title ]


viewNotFoundPage model message =
    div [ class "pv3 flex flex-column vh-100 vs3" ]
        [ div [ class "vs3 center w-90" ]
            [ viewHeader model.session
            ]
        , div [ class "flex-auto overflow-scroll" ]
            [ div [ class "center w-90" ]
                [ h1 [] [ text message ]
                ]
            ]
        ]



---- HEADER VIEW ----


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
            [ div [ class "f3 tc" ] [ H.a [ class "link black", href "/" ] [ H.text "Elm Notes" ] ]
            , viewSession
            ]



---- NOTE DETAIL PAGE ----


viewNoteDetailPage model note =
    let
        content =
            Note.getContent note
    in
        div [ class "pv3 flex flex-column vh-100 vs3" ]
            [ div [ class "vs3 center w-90" ]
                [ viewHeader model.session
                ]
            , div [ class "flex-auto overflow-scroll" ]
                [ div [ class "center w-90" ]
                    [ bbtn (RouteTo <| NoteEdit note.id) "Edit"
                    , div [ class " pv2 pointer " ] [ div [] <| Markdown.toHtml Nothing content ]
                    ]
                ]
            ]



---- NOTE Edit PAGE ----


viewNoteEditPage model pageModel =
    div [ class "pv3 flex flex-column vh-100 vs3" ]
        [ div [ class "vs3 center w-90" ]
            [ viewHeader model.session
            ]
        , div [ class "flex-grow-1 flex flex-row justify-center" ]
            [ div [ class "w-90" ]
                [ textarea
                    [ class "pa2 h-100 w-100"
                    , value <| EditNote.content pageModel
                    , onInput (EditNote.ContentChanged >> EditNotePageMsg)
                    ]
                    []
                ]
            ]
        ]



---- NOTE LIST PAGE ----


viewNoteListPage model =
    div
        [ class "pv3 flex flex-column vh-100 vs3"
        ]
        [ div [ class "vs3 center w-90" ]
            [ viewHeader model.session
            ]
        , div [ class "flex-auto overflow-scroll" ]
            [ div [ class "center w-90" ]
                [ viewNoteList (currentNoteList model)
                ]
            ]
        ]


viewNoteListDisplayItem note =
    let
        lines =
            Note.getContent note |> String.trim |> String.lines

        firstLine =
            List.head lines |> Maybe.withDefault "<Empty>"

        otherLines =
            List.tail lines |> Maybe.withDefault [] |> String.join " " |> String.slice 0 100

        routeToNoteDetailViewMsg =
            RouteTo <| NoteDetail note.id
    in
        div
            [ onClick routeToNoteDetailViewMsg
            , Exts.Html.Events.onEnter routeToNoteDetailViewMsg
            , class "link black pv2 pointer "
            , tabindex 0
            ]
            [ div [ class "f5" ] [ text firstLine ]
            , div [ class "f6 truncate black-60" ] [ text otherLines ]
            ]


viewNoteList notes =
    let
        viewItem note =
            div [ class "bb b--black-10 pv2" ]
                [ viewNoteListDisplayItem note
                ]
    in
        div [ class "pv1 vs1" ] <| List.map viewItem notes



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
