module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Color exposing (Color, hsla)
import Html exposing (Html, button, div, h1, img, input, text)
import Html.Attributes exposing (class, placeholder, src, style, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { counter : Int, content : String }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      , content = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Increment
    | Decrement
    | Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        Change newContent ->
            ( { model | content = newContent }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        [ class "pa2 bg-light-red vh-100 vw-100"
        , style "background-color" "hsla( 10 , 100% , 68% , 1)"
        , style "background-color" (hsla 0.6 0.95 0.68 1 |> Color.toCssString)
        ]
        [ div [ class "f2 pa2" ] [ text "Your Elm App is working! with hmr!" ]
        , div [ class "pa3" ]
            [ div [ class "pl3" ] [ text (String.fromInt model.counter) ]
            , button [ onClick Decrement ] [ text "-" ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        , div [ class "pa3" ]
            [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
            , div [] [ text (String.reverse model.content) ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
