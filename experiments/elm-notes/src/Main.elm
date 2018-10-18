module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Encode as E
import Browser.Navigation as Nav
import Browser
import Url exposing (Url)


-- Routing
-- Page


type Page
    = NotFound



---- MODEL ----


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : Page
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , page = NotFound
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        []


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
        ( m2, Cmd.batch [ c1, c2 ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlRequested urlReq ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )



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
