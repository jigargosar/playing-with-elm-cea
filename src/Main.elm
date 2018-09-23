module Main exposing (main)

import Browser
import Color
import Element exposing (Element, alignRight, clip, column, el, explain, fill, height, minimum, rgb, rgb255, row, scrollbarY, shrink, width)
import Element.Background
import Element.Border
import Html exposing (Html, button, col, div, h1, h3, img, input, text)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Task
import Time



---- MODEL ----


type alias Model =
    { counter : Int
    , content : String
    , name : String
    , password : String
    , passwordAgain : String
    , dieFace : Int
    , zone : Time.Zone
    , time : Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      , content = ""
      , name = ""
      , password = ""
      , passwordAgain = ""
      , dieFace = 1
      , zone = Time.utc
      , time = Time.millisToPosix 0
      }
    , Task.perform AdjustTimeZone Time.here
    )



---- UPDATE ----


type Msg
    = Increment
    | Decrement
    | Change String
    | Name String
    | Password String
    | PasswordAgain String
    | Roll
    | NewFace Int
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


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

        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( { model | dieFace = newFace }
            , Cmd.none
            )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



--- View Layout Test ---


view : Model -> Html Msg
view model =
    Element.layout
        [ clip, height fill, width fill, explain Debug.todo ]
        (column [ width fill, height fill ]
            [ viewHeader
            , row [ width fill, height fill ]
                [ viewSidebar
                , viewContent
                ]
            , viewFooter
            ]
        )


viewHeader =
    row [ width fill, explain Debug.todo ] [ Element.text "Header" ]


viewSidebar =
    row
        [ explain Debug.todo
        , Element.fillPortion 1 |> height
        , width (shrink |> minimum 200)
        ]
        [ Element.text "Sidebar" ]


viewContent =
    row
        [ explain Debug.todo
        , Element.fill |> height
        , Element.fill |> width
        ]
        [ Element.text "Content" ]


viewFooter =
    row [ width fill, explain Debug.todo ] [ Element.text "Footer" ]



---- Element VIEW ----
--          Element.Background.color (rgb255 96 158 251)


view_ : Model -> Html Msg
view_ model =
    Element.layout
        [ clip, height fill ]
        (view2 model)


view2 model =
    column
        [ class "pa2 vh-100 vw-100" |> Element.htmlAttribute ]
        ([ div [ class "f2 pa2" ] [ text "Your Elm App is working! with hmr!" ]
         , div [ class "pa3" ]
            [ div [ class "pl3 f3" ] [ text (String.fromInt model.counter) ]
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
         , div [ class "pa3" ]
            [ div [ class "f3" ] [ text (String.fromInt model.dieFace) ]
            , button [ onClick Roll ] [ text "Roll" ]
            ]
         , viewClock model
         , viewClock model
         , viewClock model
         , viewClock model
         , viewClock model
         , viewClock model
         , viewClock model
         , viewClock model
         , viewClock model
         , viewClock model
         , viewClock model
         ]
            |> List.map Element.html
        )



---- Element VIEW ----


elementView : Html msg
elementView =
    Element.layout
        [ Element.Background.color (rgb255 96 158 251) ]
        myRowOfStuff


myRowOfStuff =
    column [ width fill ]
        [ myElement
        , myElement
        , el [ alignRight ] myElement
        ]


myElement : Element msg
myElement =
    el
        [ width fill ]
        (Element.text "You've made a stylish element!")



---- HTML VIEW ----


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.password == model.passwordAgain then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]


viewClock : Model -> Html Msg
viewClock model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)
    in
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
