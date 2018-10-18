module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Encode as E
import Browser.Navigation as Nav
import Browser
import Session exposing (Session)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, top)


-- Routing
-- Page


type Page
    = NotFound Session
    | Home Session



---- MODEL ----


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    stepUrl url
        { key = key
        , page = NotFound Session.empty
        }



---- UPDATE ----


exit model =
    case model.page of
        NotFound session ->
            session

        Home session ->
            session


type alias HomeModel =
    Session


type alias HomeMsg =
    Msg


stepHome : Model -> ( HomeModel, Cmd HomeMsg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmd ) =
    ( { model | page = Home home }
    , cmd
    )


initHome : Session -> ( HomeModel, Cmd HomeMsg )
initHome session =
    ( session, Cmd.none )


stepUrl : Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        parser =
            oneOf
                [ route top
                    (stepHome model (initHome session))
                ]
    in
        case Parser.parse parser url of
            Just answer ->
                answer

            Nothing ->
                ( { model | page = NotFound session }
                , Cmd.none
                )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


type Msg
    = NoOp
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlRequested urlReq ->
            case urlReq of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Notes", body = [ htmlView model ] }


htmlView model =
    div [ class "" ] [ text "SPA" ]



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
