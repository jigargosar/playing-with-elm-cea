port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Return
import Round
import Set
import String exposing (String)
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


type Direction
    = Left
    | Right
    | Up
    | Down


type alias KeyDownSet =
    Set.Set String


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
    , keyDownSet : KeyDownSet
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
    update Cache
        (Model
            bgColor
            ballColor
            ( 100, 100 )
            (Particle ( 100, 100 ) ( 0, 0 ))
            Set.empty
        )



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

                speed =
                    20

                newVelocity =
                    case k of
                        "ArrowDown" ->
                            Just ( 0, 1 )

                        "ArrowUp" ->
                            Just ( 0, -1 )

                        "ArrowLeft" ->
                            Just ( -1, 0 )

                        "ArrowRight" ->
                            Just ( 1, 0 )

                        " " ->
                            Just ( 0, 0 )

                        _ ->
                            Nothing

                newBall =
                    newVelocity
                        |> Maybe.map
                            (Tuple.mapBoth ((*) speed) ((*) speed)
                                >> (\v -> setVelocity v m.ball)
                            )
                        |> Maybe.withDefault m.ball

                _ =
                    newVelocity |> Maybe.map (Debug.log "newVelocity")
            in
            ( { m | ball = newBall, keyDownSet = m.keyDownSet |> Set.insert k }, Cmd.none )

        KeyUp k ->
            let
                _ =
                    Debug.log "key: KeyUp" k
            in
            ( { m | keyDownSet = m.keyDownSet |> Set.remove k }, Cmd.none )


setVelocity v par =
    { par | v = v }


updateParticle delta par =
    let
        ( dx, dy ) =
            par.v
                |> Tuple.mapBoth ((*) (1.0 / 60)) ((*) (1.0 / 60))

        ( x, y ) =
            par.p

        --        _ =
        --            Debug.log "updateParticle" par
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


svgView { bgColor, ballColor, ballXY, ball } =
    let
        w =
            500

        h =
            round (w * 2 / 3)

        ballRadius =
            10

        ( bxS, byS ) =
            ball.p
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


viewKeys { keyDownSet } =
    let
        surroundString s e v =
            s ++ v ++ e

        keys =
            keyDownSet |> Set.toList |> String.join "','" |> surroundString "downKeys: '" "'"
    in
    H.div [] [ H.text keys ]


mapEach fn t =
    t |> Tuple.mapBoth fn fn


viewBallStats { ball } =
    let
        roundTuple =
            mapEach (Round.round 0)

        ( x, y ) =
            roundTuple ball.p

        ( dx, dy ) =
            roundTuple ball.v

        p =
            [ "p(", x, ",", y, ")", ", v(", dx, ",", dy, ")" ] |> String.join ""
    in
    H.div [ HA.class "code" ] [ H.text p ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , viewColors model
            , viewBallStats model
            , H.div [ HA.class "" ] [ svgView model ]
            , viewKeys model
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
