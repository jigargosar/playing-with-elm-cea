module Main exposing (..)

import Browser as B
import Browser.Events as BE
import Html as H exposing (Html)
import Html.Attributes as HA
import Ramda exposing (tupleToList)
import Size
import Svg.Attributes as SA
import Html.Lazy as HL
import Set exposing (Set)
import Json.Decode as D
import Json.Encode as E
import Svg as S


---- MODEL ----


type alias Model =
    { keySet : Set String
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    { keySet = Set.empty } |> noCmd



---- UPDATE ----


type Msg
    = NoOp
    | AnimationFrame Float
    | KeyDown String
    | KeyUp String


noCmd model =
    ( model, Cmd.none )


addCmd c2 =
    Tuple.mapSecond (\c1 -> Cmd.batch [ c1, c2 ])


withCmd c m =
    noCmd m |> addCmd c


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            noCmd m

        AnimationFrame elapsed ->
            noCmd m

        KeyDown key ->
            { m | keySet = Set.insert key m.keySet } |> noCmd

        KeyUp key ->
            { m | keySet = Set.remove key m.keySet } |> noCmd



---- VIEW ----


worldSize =
    Size.fromComponent ( 600, 350 )


worldSizeIntT =
    Size.toRoundIntComponent worldSize


view : Model -> Html Msg
view m =
    H.div [ HA.class "flex flex-column items-center pa2 h-100 " ]
        [ H.div [ HA.class "flex flex-column vs3" ]
            [ H.div [ HA.class "f3" ] [ H.text "SVG API" ]
            , viewSvg m
            ]
        ]


viewSvg m =
    let
        attrs =
            worldSizeIntT |> Tuple.mapBoth HA.width HA.height |> tupleToList
    in
        S.svg attrs
            [ S.rect
                [ SA.width "100%"
                , SA.height "100%"
                , SA.strokeWidth "0.2"
                , SA.stroke "#000"
                , SA.fill "lightblue"
                ]
                []
            , viewGameContent m
            ]


viewGameContent m =
    S.g [] []



---- PROGRAM ----


subscriptions _ =
    Sub.batch
        [ BE.onAnimationFrameDelta AnimationFrame
        , BE.onKeyDown (D.map KeyDown (D.field "key" D.string))
        , BE.onKeyUp (D.map KeyUp (D.field "key" D.string))
        ]


main : Program Flags Model Msg
main =
    B.element
        { view = HL.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
