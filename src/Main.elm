module Main exposing (main)

import Browser
import Color
import Element exposing (Element, alignRight, behindContent, centerX, centerY, clip, column, el, explain, fill, fillPortion, height, maximum, minimum, padding, paddingXY, px, rgb, rgb255, rgba, row, scrollbarY, scrollbars, shrink, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, button, col, div, h1, h3, img, input)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Round
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
    , red : Float
    , green : Float
    , blue : Float
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
      , red = 1
      , green = 1
      , blue = 1
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
    | Nop
    | Red Float
    | Green Float
    | Blue Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Red newRed ->
            ( { model | red = newRed }, Cmd.none )

        Green newGreen ->
            ( { model | green = newGreen }, Cmd.none )

        Blue newBlue ->
            ( { model | blue = newBlue }, Cmd.none )

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


pXY : Int -> Int -> Element.Attribute msg
pXY x y =
    paddingXY (scaledInt x) (scaledInt y)


hsl : Float -> Float -> Float -> Float -> Element.Color
hsl h s l a =
    Color.hsla h s l a
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } -> rgba red green blue alpha)


view : Model -> Html Msg
view model =
    Element.layout
        [ fz 1
        , height fill
        , width fill
        , style "background-color" "lightgray" |> Element.htmlAttribute
        ]
        (column
            [ height fill, width fill, clip ]
            [ el
                [ p 1
                , fz 1
                , width fill
                , height fill
                , scrollbars
                ]
                (el
                    [ width fill
                    , Background.color (rgb model.red model.green model.blue)
                    , Border.shadow
                        { offset = ( 2, 2 )
                        , size = 0
                        , blur = 4
                        , color = rgba 0 0 0 0.4
                        }
                    ]
                    (column [ height fill, width fill, p 1 ]
                        [ el
                            [ p 1, centerX, fz 4 ]
                            (text "Color Converter")
                        , el
                            [ width fill, height fill ]
                            (svgView |> Element.html)
                        ]
                    )
                )
            , viewKnobs model
            ]
        )


viewKnobs : Model -> Element Msg
viewKnobs model =
    column
        [ width fill
        , height (fill |> maximum 250 |> minimum 100)
        , width fill
        , style "background-color" "hsl(0,0%,10%)" |> Element.htmlAttribute
        , style "color" "hsl(0,0%,90%)" |> Element.htmlAttribute
        , p 1
        , spacing 16
        , Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
        , Border.shadow
            { offset = ( 0, -2 )
            , size = 0
            , blur = 16
            , color = rgba 0 0 0 0.4
            }
        ]
        [ text "Controller"
        , colorSlider model.red "R" Red
        , colorSlider model.green "G" Green
        , colorSlider model.blue "B" Blue
        ]


colorSlider channelFloatValue labelText onChange =
    row [ spacing 16, width fill ]
        [ Input.slider
            [ spacing 16
            , behindContent
                (el
                    [ width fill
                    , height (px 2)
                    , centerY
                    , Background.color (rgb 0.5 0.5 0.5)
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { onChange = onChange
            , label = Input.labelLeft [] (text labelText)
            , min = 0
            , max = 1
            , step = Just 0.01
            , value = channelFloatValue
            , thumb =
                Input.defaultThumb
            }
        , Input.text
            [ spacing 0
            , style "background-color" "hsl(0,0%,10%)" |> Element.htmlAttribute
            , style "color" "hsl(0,0%,90%)" |> Element.htmlAttribute
            , Html.Attributes.type_ "number" |> Element.htmlAttribute
            , Html.Attributes.step "0.01" |> Element.htmlAttribute
            , Html.Attributes.min "0" |> Element.htmlAttribute
            , Html.Attributes.max "1" |> Element.htmlAttribute
            ]
            { onChange =
                \val ->
                    let
                        maybeRed =
                            String.toFloat val
                    in
                    maybeRed
                        |> Maybe.map (\red -> red |> clamp 0 1 |> onChange)
                        |> Maybe.withDefault Nop
            , label = Input.labelLeft [] Element.none
            , text = channelFloatValue |> Round.round 2
            , placeholder = Nothing
            }
        ]


svgView : Html msg
svgView =
    Svg.svg
        [ SA.id "svg-demo-1"
        , SA.width "100%"

        --        , SA.height "100%"
        , SA.viewBox "0 0 500 100"
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
