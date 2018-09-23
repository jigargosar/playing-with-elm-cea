module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Color exposing (Color, hsla)
import Html exposing (Html, button, div, h1, img, input, text)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { counter : Int
    , content : String
    , name : String
    , password : String
    , passwordAgain : String
    }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      , content = ""
      , name = ""
      , password = ""
      , passwordAgain = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Increment
    | Decrement
    | Change String
    | Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        Change newContent ->
            ( { model | content = newContent }, Cmd.none )

        Name name ->
            ( { model | name = name }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        PasswordAgain password ->
            ( { model | passwordAgain = password }, Cmd.none )



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
        , div [ class "pa3" ]
            [ viewInput "text" "Name" model.name Name
            , viewInput "password" "Password" model.password Password
            , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
            , viewValidation model
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.password == model.passwordAgain then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
