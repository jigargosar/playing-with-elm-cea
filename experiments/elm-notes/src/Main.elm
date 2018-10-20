module Main exposing (main)

import Auth exposing (AuthState)
import Browser
import Browser.Navigation as Nav
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import MagicMenu exposing (MagicMenu)
import Page.Note
import Page.Notes as Notes
import Ports
import Random
import Session exposing (Session)
import Skeleton exposing (defaultSkeletonDetails)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string, top)
import Url.Parser.Query as Query



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
    | Note Page.Note.Model



---- MODEL ----


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { url : Url
    , key : Nav.Key
    , page : Page
    , authState : AuthState
    , magicMenu : MagicMenu
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
                , magicMenu = { open = False }
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


stepNote : Model -> ( Page.Note.Model, Cmd Page.Note.Msg ) -> ( Model, Cmd Msg )
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
                , route (s "notes" </> s "new")
                    (stepNote model (Page.Note.initNewNote session))
                , route (s "notes" </> string <?> Query.string "edit")
                    (\id maybeEdit ->
                        case maybeEdit of
                            Just _ ->
                                stepNote model (Page.Note.initEditNote id session)

                            Nothing ->
                                stepNote model (Page.Note.initShowNote id session)
                    )
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
    | NoteMsg Page.Note.Msg
    | MagicMenuMsg MagicMenu.Msg
    | MBClicked
    | Back
    | Forward
    | Home


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Auth.subscriptions model.authState |> Sub.map AuthMsg
        , Notes.subscriptions |> Sub.map NotesMsg
        ]


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

        MagicMenuMsg msg ->
            stepMagicMenu model (MagicMenu.update msg model.magicMenu)

        NotesMsg msg ->
            case model.page of
                Notes notes ->
                    stepNotes model (Notes.update msg notes)

                _ ->
                    ( model, Cmd.none )

        NoteMsg msg ->
            case model.page of
                Note note ->
                    stepNote model (Page.Note.update msg note)

                _ ->
                    ( model, Cmd.none )

        MBClicked ->
            ( model |> overMagicMenu (overOpen not), Cmd.none )

        Back ->
            ( model, Nav.back model.key 1 )

        Forward ->
            ( model, Nav.forward model.key 1 )

        Home ->
            ( model, Nav.pushUrl model.key Href.home )


overMagicMenu : (MagicMenu -> MagicMenu) -> Model -> Model
overMagicMenu updateFn model =
    { model | magicMenu = updateFn model.magicMenu }


overOpen : (Bool -> Bool) -> MagicMenu -> MagicMenu
overOpen updateFn model =
    { model | open = updateFn model.open }


stepAuth model ( authState, cmd ) =
    ( { model | authState = authState }, Cmd.map AuthMsg cmd )


stepMagicMenu model ( magicMenu, cmd ) =
    ( { model | magicMenu = magicMenu }, Cmd.map MagicMenuMsg cmd )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        config : Skeleton.MainDetails Msg
        config =
            { authState = model.authState
            , toAuthMsg = AuthMsg
            , mbClickedMsg = MBClicked
            , magicMenu = model.magicMenu
            , magicMenuNavActions =
                { back = Back
                , forward = Forward
                , home =
                    if model.url.path == "/" then
                        NoOp

                    else
                        Home
                }
            , toMagicMenuMsg = MagicMenuMsg
            , back = Back
            , forward = Forward
            , home =
                if model.url.path == "/" then
                    NoOp

                else
                    Home
            }
    in
    case model.page of
        NotFound _ ->
            Skeleton.view config
                identity
                { defaultSkeletonDetails
                    | title = "Not Found"
                    , kids = [ div [] [ text "404 Page Not Found" ] ]
                }

        Notes notes ->
            Skeleton.view config NotesMsg (Notes.view notes)

        Note note ->
            Skeleton.view config NoteMsg (Page.Note.view note)
