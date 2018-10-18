module Main exposing (..)

import Auth exposing (AuthState)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Browser.Navigation as Nav
import Browser
import Ports
import Session exposing (Session)
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


type alias HomeModel =
    Session


type alias HomeMsg =
    Msg


initHome : Session -> ( HomeModel, Cmd HomeMsg )
initHome session =
    ( session, Cmd.none )


type Page
    = NotFound Session
    | Home Session



---- MODEL ----


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { key : Nav.Key
    , page : Page
    , authState : AuthState
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    stepUrl url
        { key = key
        , page = NotFound Session.empty
        , authState = Auth.init
        }



---- UPDATE ----


exit model =
    case model.page of
        NotFound session ->
            session

        Home session ->
            session


stepHome : Model -> ( HomeModel, Cmd HomeMsg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmd ) =
    ( { model | page = Home home }
    , cmd
    )


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
    | AuthMsg Auth.Msg


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
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        AuthMsg msg ->
            stepAuth model (Auth.update msg model.authState)


stepAuth model ( authState, cmd ) =
    ( { model | authState = authState }, Cmd.map AuthMsg cmd )



---- VIEW ----


type alias SkeletonDetails msg =
    { title : String
    , header : List String
    , warning : List String
    , attrs : List (Html.Attribute msg)
    , kids : List (Html msg)
    }


view : Model -> Browser.Document Msg
view model =
    let
        config =
            { authState = model.authState }
    in
        case model.page of
            NotFound _ ->
                skeletonView config
                    identity
                    { title = "Not Found"
                    , header = []

                    {- , warning = Skeleton.NoProblems
                       , attrs = Problem.styles
                       , kids = Problem.notFound
                    -}
                    , warning = []
                    , attrs = []
                    , kids = [ div [] [ text "Not Found View" ] ]
                    }

            Home home ->
                skeletonView config identity (homeView home)


homeView : HomeModel -> SkeletonDetails HomeMsg
homeView model =
    { title = "Elm Packages"
    , header = []
    , warning = []
    , attrs = []
    , kids =
        [ div [] [ text "Home View" ] ]
    }


type alias SkeletonConfig =
    { authState : AuthState }


skeletonView : SkeletonConfig -> (a -> Msg) -> SkeletonDetails a -> Browser.Document Msg
skeletonView config toMsg details =
    { title =
        details.title
    , body =
        [ {- viewHeader details.header
             , lazy viewWarning details.warning
             ,
          -}
          viewHeader config.authState
        , Html.map
            toMsg
          <|
            div (class "center" :: details.attrs) details.kids

        --        , viewFooter
        ]
    }


rowS3 classes attrs =
    div (class ("flex flex-row hs3 items-center " ++ classes) :: attrs)


row =
    rowS3


txtA attrs content =
    row "" attrs [ text content ]


txt =
    txtA []


viewHeader auth =
    div [ class "bg-black white" ]
        [ row "b _bg-white-50 center pv3 ph3 ph0-l justify-between measure-wide shadow-1"
            []
            [ txtA [] "ELM Notes"
            , case auth of
                Auth.Authenticated { displayName, photoUrl } ->
                    viewAuthAvatarBtn (Just Auth.SignOutClicked) (Just photoUrl) displayName

                Auth.InitialUnknown ->
                    viewAuthAvatarBtn Nothing Nothing "Loading"

                Auth.Anon ->
                    viewAuthAvatarBtn (Just Auth.SignInClicked) Nothing "Anon"
            ]
            |> Html.map AuthMsg
        ]


viewAuthAvatarBtn maybeOnClick maybePhotoUrl textContent =
    let
        onClickAttr =
            Maybe.map (onClick >> List.singleton) maybeOnClick |> Maybe.withDefault []

        photoUrl =
            Maybe.withDefault "anon.svg" maybePhotoUrl
    in
        row "pointer"
            onClickAttr
            [ txt textContent
            , img [ width 24, class "br-pill", src photoUrl ] []
            ]
