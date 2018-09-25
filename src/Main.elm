module Main exposing (main)

import Browser
import Color
import ColorX
import Element exposing (Element, alignRight, behindContent, centerX, centerY, clip, column, el, explain, fill, fillPortion, height, padding, paddingXY, px, rgb, rgb255, rgba, row, scrollbarY, scrollbars, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import ElementX exposing (bc, bcInherit, brc, fc, fz, grayscale, hsla, inputNumber, labelNone, lightGray, maxRem, minRem, p, pXY, scaledInt, spRem, white)
import Hex
import Html exposing (Html, button, col, div, h1, h3, img, input)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import RGBA exposing (RGBA)
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
    , alpha : Float
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
      , alpha = 1
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
    | Alpha Float
    | Hue Float
    | Saturation Float
    | Lightness Float


modelToHSLA model =
    Color.rgba model.red model.green model.blue model.alpha
        |> Color.toHsla


modelToHEXA : Model -> String
modelToHEXA =
    RGBA.fromPartial >> RGBA.toHexAString


updateHSLA fn model =
    model
        |> modelToHSLA
        |> fn
        |> Color.fromHsla
        |> Color.toRgba
        |> (\r -> { model | red = r.red, green = r.green, blue = r.blue, alpha = r.alpha })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        foo =
            Debug.log "Msg" msg
    in
    case msg of
        Nop ->
            ( model, Cmd.none )

        Red newRed ->
            ( { model | red = newRed }, Cmd.none )

        Green newGreen ->
            ( { model | green = newGreen }, Cmd.none )

        Blue newBlue ->
            ( { model | blue = newBlue }, Cmd.none )

        Alpha newAlpha ->
            ( { model | alpha = newAlpha }, Cmd.none )

        Hue newHue ->
            let
                ( newSaturation, newLightness ) =
                    let
                        { hue, saturation, lightness } =
                            modelToHSLA model
                    in
                    if newHue > 0 && saturation <= 0 && lightness >= 1 then
                        ( 0.01, 0.99 )

                    else
                        ( saturation, lightness )
            in
            ( model
                |> updateHSLA
                    (\r ->
                        { r
                            | hue = newHue
                            , saturation = newSaturation
                            , lightness = newLightness
                        }
                    )
            , Cmd.none
            )

        Saturation newSaturation ->
            let
                ( newHue, newLightness ) =
                    let
                        { hue, saturation, lightness } =
                            modelToHSLA model
                    in
                    if hue <= 0 && newSaturation > 0 && lightness >= 1 then
                        ( 0.01, 0.99 )

                    else
                        ( hue, lightness )
            in
            ( model
                |> updateHSLA
                    (\r ->
                        { r
                            | hue = newHue
                            , saturation = newSaturation
                            , lightness = newLightness
                        }
                    )
            , Cmd.none
            )

        Lightness newLightness ->
            ( model |> updateHSLA (\r -> { r | lightness = newLightness }), Cmd.none )

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


fillWH : List (Element.Attribute msg)
fillWH =
    [ height fill
    , width fill
    ]


clipFillWH : List (Element.Attribute msg)
clipFillWH =
    [ height fill
    , width fill
    , clip
    ]


scrollFillWH : List (Element.Attribute msg)
scrollFillWH =
    [ height fill
    , width fill
    , scrollbars
    ]


elevation1 : Element.Attr decorative msg
elevation1 =
    Border.shadow
        { offset = ( 2, 2 )
        , size = 0
        , blur = 4
        , color = rgba 0 0 0 0.4
        }


view : Model -> Html Msg
view model =
    Element.layout
        (List.concat
            [ fillWH
            , [ fz 1
              , Background.color lightGray
              ]
            ]
        )
        (column
            (List.concat [ clipFillWH ])
            [ el
                (List.concat [ [ p 1 ], scrollFillWH ])
                (el
                    [ width fill
                    , bc (rgba model.red model.green model.blue model.alpha)

                    --                    , elevation1
                    , Html.Attributes.class "mdc-elevation--z24" |> Element.htmlAttribute
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


withAttributes :
    List b
    -> (List b -> a)
    -> List b
    -> a
withAttributes l1 element l2 =
    element (List.append l1 l2)


viewKnobs : Model -> Element Msg
viewKnobs model =
    let
        { hue, saturation, lightness } =
            modelToHSLA model

        conf =
            { value = 0.0, labelText = "", onChange = \_ -> Nop, max = 1 }

        each =
            { bottom = 0, top = 0, left = 0, right = 0 }
    in
    column
        [ width fill
        , height (fill |> maxRem 20)
        , bc (grayscale 0.1)
        , fc (grayscale 0.9)
        , Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
        , Border.shadow
            { offset = ( 0, -2 )
            , size = 0
            , blur = 16
            , color = rgba 0 0 0 0.4
            }
        ]
        [ el
            [ width fill
            , p 1
            , brc white
            , Border.widthEach { each | bottom = 1 }
            ]
            (text "Controller")
        , viewColorSliders model
        ]


viewColorSliders model =
    let
        conf =
            { value = 0.0, labelText = "", onChange = \_ -> Nop, max = 1 }

        alphaSlider =
            colorSlider { conf | value = model.alpha, labelText = "alpha", onChange = Alpha }

        column1 =
            column
                [ spRem 1, width fill ]
                (rgbSliders model)

        column2 =
            column
                [ spRem 1, width fill ]
                (hslSliders model)

        row1 =
            row [ width fill, spRem 1 ] [ column1, column2 ]

        row2 =
            row [ width fill, spRem 1 ]
                [ alphaSlider
                , el [ width fill, fz 2 ]
                    (modelToHEXA model |> text)
                ]
    in
    el
        ([ p 1 ] ++ scrollFillWH)
        (column (fillWH ++ [ spRem 1 ]) [ row1, row2 ])


hslSliders model =
    let
        { hue, saturation, lightness } =
            modelToHSLA model

        conf =
            { value = 0.0, labelText = "", onChange = \_ -> Nop, max = 1 }
    in
    [ { conf | value = hue, labelText = "hue", onChange = Hue, max = 0.99 }
    , { conf | value = saturation, labelText = "saturation", onChange = Saturation }
    , { conf | value = lightness, labelText = "lightness", onChange = Lightness }
    ]
        |> List.map colorSlider


rgbSliders model =
    let
        { hue, saturation, lightness } =
            modelToHSLA model

        conf =
            { value = 0.0, labelText = "", onChange = \_ -> Nop, max = 1 }
    in
    [ { conf | value = model.red, labelText = "red", onChange = Red }
    , { conf | value = model.green, labelText = "green", onChange = Green }
    , { conf | value = model.blue, labelText = "blue", onChange = Blue }
    ]
        |> List.map colorSlider


colorSlider :
    { value : Float
    , labelText : String
    , onChange : Float -> msg
    , max : Float
    }
    -> Element msg
colorSlider { onChange, labelText, value, max } =
    let
        min =
            0

        step =
            0.01

        finalLabelText =
            labelText |> String.left 1 |> String.toUpper
    in
    row [ spRem 1, width fill ]
        [ Input.slider
            [ spRem 1
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
            , label =
                Input.labelLeft
                    []
                    (el [ alignRight ] (text finalLabelText))
            , min = min
            , max = max
            , step = Just step
            , value = value
            , thumb =
                Input.defaultThumb
            }
        , inputNumber
            [ spRem 0, p -4 ]
            { onChange = onChange
            , min = min
            , max = max
            , step = step
            , round = 2
            , label = labelNone
            , value = value
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
