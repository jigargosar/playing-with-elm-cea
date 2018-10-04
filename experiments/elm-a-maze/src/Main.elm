module Main exposing (..)

import Color
import Html as H exposing (Html)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Ramda as R
import Size
import Svg as S
import TypedSvg as T
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as T
import TypedSvg.Attributes.InPx as TP
import Svg.Attributes as S
import Svg.Attributes as SA
import Browser as B
import Browser.Events as B
import Browser.Events as BE
import Set exposing (Set)
import Json.Decode as D
import Json.Encode as E
import TypedSvg.Types exposing (Fill(..), px)


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


fillColor =
    Fill >> TA.fill


fillOpacityFloat =
    TypedSvg.Types.Opacity >> TA.fillOpacity


opacityFloat =
    TypedSvg.Types.Opacity >> TA.opacity


viewSvg m =
    let
        attrs =
            worldSizeIntT |> Tuple.mapBoth HA.width HA.height |> R.tupleToList
    in
        S.svg attrs
            [ S.rect
                [ S.width "100%"
                , S.height "100%"
                , TA.strokeWidth (px 0.2)
                , TA.stroke Color.black
                , Color.toHsla Color.blue |> \h -> { h | saturation = 1, lightness = 0.7 } |> Color.fromHsla >> fillColor
                , fillOpacityFloat 1

                --                , opacityFloat 0.5
                ]
                []
            , viewGameContent m
            ]


viewGameContent m =
    S.g [] []



---- PROGRAM ----


subscriptions _ =
    Sub.batch
        [ B.onAnimationFrameDelta AnimationFrame
        , B.onKeyDown (D.map KeyDown (D.field "key" D.string))
        , B.onKeyUp (D.map KeyUp (D.field "key" D.string))
        ]


main : Program Flags Model Msg
main =
    B.element
        { view = H.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
