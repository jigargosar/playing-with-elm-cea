module Cards exposing (main)

import Color exposing (Color)
import Html
import Html as H exposing (Html)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Light
import Ramda as R
import Size
import Svg
import Svg as S
import Svg.Attributes as S
import Svg.Attributes as SA
import TypedSvg as T
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as T
import TypedSvg.Attributes.InPx as TP
import Browser as B
import Browser.Events as B
import Browser.Events as BE
import Set exposing (Set)
import Json.Decode as D
import Json.Encode as E
import TypedSvg.Types exposing (Fill(..), px)
import UiCards exposing (card, cardError, deck, show)


type alias Model =
    {}


type Msg
    = NoOp


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


initialModel =
    {}


main =
    show update
        [ deck "Card elements"
            [ card "Card" initialModel <|
                \model ->
                    H.div [ H.class "flex flex-column vs3" ]
                        [ H.div [ H.class "f2" ] [ Html.text "CARD" ]
                        , H.div [ H.class "" ] [ Html.text "I am a text " ]
                        ]
            , card "Error test" initialModel <|
                \_ ->
                    cardError "This is a test"
            ]
        ]
