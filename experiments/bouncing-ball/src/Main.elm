port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Return
import Svg
import Svg.Attributes as SA



---- PORT ----


port cache : E.Value -> Cmd msg



---- MODEL ----


type alias Point =
    ( Float, Float )


type alias Velocity =
    ( Float, Float )


type alias Particle =
    { p : Point, v : Velocity }


type alias Flags =
    { cache : E.Value }


type alias CacheModel =
    { bgColor : String
    , ballColor : String
    }


type alias Model =
    { bgColor : String
    , ballColor : String
    , ballXY : ( Float, Float )
    , ball : Particle
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        { bgColor, ballColor } =
            D.decodeValue
                (D.map2 CacheModel
                    (D.field "bgColor" D.string)
                    (D.field "ballColor" D.string)
                )
                flags.cache
                |> Result.mapError (Debug.log "Error decoding flags.cache")
                |> Result.withDefault (CacheModel "#adbeeb" "#cd37a9")
    in
    update Cache (Model bgColor ballColor ( 100, 100 ) (Particle ( 100, 100 ) ( 0, 0 )))



---- UPDATE ----


type Msg
    = NoOp
    | BgColor String
    | BallColor String
    | Cache
    | AFrame Float
    | KeyPress String
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        BgColor bgColor ->
            ( { m | bgColor = bgColor }, Cmd.none )

        BallColor ballColor ->
            ( { m | ballColor = ballColor }, Cmd.none )

        Cache ->
            ( m, cacheModel m )

        AFrame delta ->
            ( { m | ball = updateParticle delta m.ball }
            , Cmd.none
            )

        KeyPress k ->
            let
                _ =
                    Debug.log "key: KeyPress" k
            in
            ( m, Cmd.none )

        KeyDown k ->
            let
                _ =
                    Debug.log "key: KeyDown" k
            in
            ( m, Cmd.none )

        KeyUp k ->
            let
                _ =
                    Debug.log "key: KeyUp" k
            in
            ( m, Cmd.none )


updateParticle delta par =
    let
        ( dx, dy ) =
            par.v

        ( x, y ) =
            par.p
    in
    { par | p = ( x + dx, y + dy ) }


cacheModel { ballColor, bgColor } =
    E.object
        [ ( "ballColor", E.string ballColor )
        , ( "bgColor", E.string bgColor )
        ]
        |> cache



---- VIEW ----
---- Step 1 draw svg ----
-- * Make it pretty


green =
    "#b1ebad"


blue =
    "#4427d9"


svgView { bgColor, ballColor, ballXY } =
    let
        w =
            500

        h =
            round (w * 2 / 3)

        ballRadius =
            10

        ( bxS, byS ) =
            ballXY
                |> Tuple.mapBoth String.fromFloat String.fromFloat
    in
    Svg.svg [ HA.width w, HA.height h ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill bgColor ] []
        , Svg.circle
            [ SA.cx bxS
            , SA.cy byS
            , SA.r (ballRadius |> String.fromInt)
            , SA.fill ballColor
            ]
            []
        ]


viewColors { ballColor, bgColor } =
    H.div [ HA.class "hs3" ]
        [ H.input [ HA.type_ "color", HE.onInput BgColor, HA.value bgColor ] []
        , H.span [] [ H.text bgColor ]
        , H.input [ HA.type_ "color", HE.onInput BallColor, HA.value ballColor ] []
        , H.span [] [ H.text ballColor ]
        ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , viewColors model
            , H.div [ HA.class "" ] [ svgView model ]
            ]
        ]



---- Subscriptions ----


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string


subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AFrame
        , Browser.Events.onKeyPress (D.map KeyPress keyDecoder)
        , Browser.Events.onKeyDown (D.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (D.map KeyUp keyDecoder)
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
