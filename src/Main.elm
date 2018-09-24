module Main exposing (main)

import Browser
import Color
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , clip
        , column
        , el
        , explain
        , fill
        , fillPortion
        , height
        , maximum
        , minimum
        , padding
        , rgb
        , rgb255
        , row
        , scrollbarY
        , scrollbars
        , shrink
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background
import Element.Border
import Element.Font as Font
import Html exposing (Html, button, col, div, h1, h3, img, input)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Svg as Svg
import Svg.Attributes as SA
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



--- View ---


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


scaledInt : Int -> Int
scaledInt =
    scaled >> round


fz : Int -> Element.Attr decorative msg
fz =
    scaledInt >> Font.size


p : Int -> Element.Attribute msg
p =
    scaledInt >> padding


view : Model -> Html Msg
view model =
    Element.layout [ fz 1, height fill, width fill, clip ]
        (column
            [ height fill, width fill, clip ]
            [ el
                [ p 8
                , height fill
                , width fill
                , clip
                , scrollbars
                ]
                (el
                    [ p 8
                    , height fill
                    , width fill
                    , clip
                    , scrollbars
                    , Element.Border.width 1
                    ]
                    (column [ height fill, width fill ]
                        [ el
                            [ p 1, {- width fill, Font.center, -} Element.centerX, fz 4 ]
                            (text "Hello World")
                        , el [ width fill, height fill, Element.centerX ] (svgView |> Element.html)
                        ]
                    )
                )
            , el
                [ height (fill |> maximum 250 |> minimum 100), width fill ]
                (text "Controller")
            ]
        )


svgView =
    Svg.svg
        [ SA.id "svg-demo-1"
        , SA.width "100%"

        --        , SA.height "100%"
        , SA.viewBox "0 0 500 250"
        , SA.style "flex:1 1 auto"
        ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill "#361110" ] []
        , roundedRect
        ]


roundedRect =
    Svg.rect
        [ SA.x "100"
        , SA.y "10"
        , SA.width "100"
        , SA.height "100"
        , SA.rx "15"
        , SA.ry "15"
        , SA.color "red"
        , SA.fill "#caf3f5"
        ]
        []



--#361110 : #caf3f5
--- View Layout Test ---
--view_ : Model -> Html Msg
--view_ model =
--    let
--        createElements count =
--            el
--                []
--                (Element.text "some filler text")
--                |> List.repeat count
--
--        fillerElements =
--            createElements 50
--    in
--    layout
--        [ height fill, width fill ]
--        (column [ height fill, width fill ]
--            [ el [ padding 16, width fill, center ] <| Element.text "Header"
--
--            -- Is clip required below?
--            , row
--                [ width fill
--                , height fill
--                , scrollbars
--
--                {- , clip -}
--                ]
--                [ column
--                    [ padding 16, scrollbars, width (fill |> maximum 250), height fill ]
--                    fillerElements
--                , column
--                    [ padding 16, scrollbars, width fill, height fill ]
--                    fillerElements
--                ]
--            , el [ padding 16, width fill, center ] <| Element.text "Footer"
--            ]
--        )
--
--
--          Element.Background.color (rgb255 96 158 251)
--- Old View ---
--oldView model =
--    column
--        [ class "pa2 vh-100 vw-100" |> Element.htmlAttribute ]
--        ([ div [ class "f2 pa2" ] [ Html.text "Your Elm App is working! with hmr!" ]
--         , div [ class "pa3" ]
--            [ div [ class "pl3 f3" ] [ Html.text (String.fromInt model.counter) ]
--            , button [ onClick Decrement ] [ Html.text "-" ]
--            , button [ onClick Increment ] [ Html.text "+" ]
--            ]
--         , div [ class "pa3" ]
--            [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
--            , div [] [ Html.text (String.reverse model.content) ]
--            ]
--         , div [ class "pa3" ]
--            [ viewInput "text" "Name" model.name Name
--            , viewInput "password" "Password" model.password Password
--            , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
--            , viewValidation model
--            ]
--         , div [ class "pa3" ]
--            [ div [ class "f3" ] [ Html.text (String.fromInt model.dieFace) ]
--            , button [ onClick Roll ] [ Html.text "Roll" ]
--            ]
--         , viewClock model
--         ]
--            |> List.map Element.html
--        )
--
--
--viewInput : String -> String -> String -> (String -> msg) -> Html msg
--viewInput t p v toMsg =
--    input [ type_ t, placeholder p, value v, onInput toMsg ] []
--
--
--viewValidation : Model -> Html msg
--viewValidation model =
--    if model.password == model.passwordAgain then
--        div [ style "color" "green" ] [ Html.text "OK" ]
--
--    else
--        div [ style "color" "red" ] [ Html.text "Passwords do not match!" ]
--
--
--viewClock : Model -> Html Msg
--viewClock model =
--    let
--        hour =
--            String.fromInt (Time.toHour model.zone model.time)
--
--        minute =
--            String.fromInt (Time.toMinute model.zone model.time)
--
--        second =
--            String.fromInt (Time.toSecond model.zone model.time)
--    in
--    h1 [] [ Html.text (hour ++ ":" ++ minute ++ ":" ++ second) ]
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
