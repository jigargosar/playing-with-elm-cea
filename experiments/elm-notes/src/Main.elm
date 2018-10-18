module Main exposing (..)

import Auth exposing (AuthState)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Browser.Navigation as Nav
import Browser
import Pages.Note
import Pages.Notes as Notes
import Ports
import Random
import Session exposing (Session)
import Skeleton
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, top)


---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- Routing
-- Page


type Page
    = NotFound Session
    | Notes Notes.Model
    | Note Pages.Note.Model



---- MODEL ----


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { url : Url
    , key : Nav.Key
    , page : Page
    , authState : AuthState
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Random.step (generator url key flags.noteList) (Random.initialSeed flags.now)
        |> Tuple.first
        |> stepUrl url


generator url key encodedNC =
    Session.generator key encodedNC
        |> Random.map
            (\session ->
                { url = url
                , key = key
                , page = NotFound session
                , authState = Auth.init
                }
            )



---- UPDATE ----


exit model =
    case model.page of
        NotFound session ->
            session

        Notes m ->
            m.session

        Note m ->
            m.session


stepNotes : Model -> ( Notes.Model, Cmd Notes.Msg ) -> ( Model, Cmd Msg )
stepNotes model ( notes, cmd ) =
    ( { model | page = Notes notes }
    , Cmd.map NotesMsg cmd
    )


stepNote : Model -> ( Pages.Note.Model, Cmd Pages.Note.Msg ) -> ( Model, Cmd Msg )
stepNote model ( notes, cmd ) =
    ( { model | page = Note notes }
    , Cmd.map NoteMsg cmd
    )


stepUrl : Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        parser =
            oneOf
                [ route top
                    (stepNotes model (Notes.init session))
                ]
    in
        (case Parser.parse parser url of
            Just answer ->
                answer

            Nothing ->
                ( { model | page = NotFound session }
                , Cmd.none
                )
        )
            |> Tuple.mapFirst (setUrl url)


setUrl url model =
    { model | url = url }


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


type Msg
    = NoOp
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | AuthMsg Auth.Msg
    | NotesMsg Notes.Msg
    | NoteMsg Pages.Note.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Auth.subscriptions model.authState |> Sub.map AuthMsg ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        UrlRequested urlReq ->
            case urlReq of
                Browser.Internal url ->
                    if Url.toString model.url /= Url.toString url then
                        ( model, Nav.pushUrl model.key (Url.toString url) )
                    else
                        ( model, Cmd.none )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        AuthMsg msg ->
            stepAuth model (Auth.update msg model.authState)

        NotesMsg msg ->
            case model.page of
                Notes notes ->
                    stepNotes model (Notes.update msg notes)

                _ ->
                    ( model, Cmd.none )

        NoteMsg msg ->
            case model.page of
                Note note ->
                    stepNote model (Pages.Note.update msg note)

                _ ->
                    ( model, Cmd.none )


stepAuth model ( authState, cmd ) =
    ( { model | authState = authState }, Cmd.map AuthMsg cmd )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        config : Skeleton.AuthDetails Msg
        config =
            { authState = model.authState, toAuthMsg = AuthMsg }
    in
        case model.page of
            NotFound _ ->
                Skeleton.view config
                    identity
                    { title = "Not Found"
                    , attrs = []
                    , kids = [ div [] [ text "Not Found View" ] ]
                    }

            Notes notes ->
                Skeleton.view config NotesMsg (Notes.view notes)

            Note note ->
                Skeleton.view config NoteMsg (Pages.Note.view note)
