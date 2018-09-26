module Main exposing (main)

import Browser
import CollageExample
import Color
import Element exposing (Element, alignRight, behindContent, centerX, centerY, column, el, fill, height, px, row, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input exposing (thumb)
import ElementX
    exposing
        ( bc
        , bcInherit
        , black
        , brc
        , clipFillWH
        , elevation
        , fc
        , fillWH
        , fz
        , grayscale
        , hsla
        , inputNumber
        , labelNone
        , lightGray
        , maxRem
        , minRem
        , p
        , pXY
        , scaledInt
        , scrollFillWH
        , sp
        , white
        )
import Hex
import Hsla
import Html exposing (Html, button, col, div, h1, h3, img, input)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Rgba
import Round
import Svg as Svg
import Svg.Attributes as SA
import SvgView
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
    , hsla : Hsla.HSLA
    , rgba : Rgba.RGBA
    , isControllerCollapsed : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        initialRGBA : Rgba.RGBA
        initialRGBA =
            Rgba.create 1 1 1 1
    in
    ( { counter = 0
      , content = ""
      , name = ""
      , password = ""
      , passwordAgain = ""
      , dieFace = 1
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , rgba = initialRGBA
      , hsla = Rgba.toHSLA initialRGBA
      , isControllerCollapsed = False
      }
    , Task.perform AdjustTimeZone Time.here
    )



---- UPDATE ----


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Nop
    | Red Float
    | Green Float
    | Blue Float
    | Alpha Float
    | Hue Float
    | Saturation Float
    | Lightness Float
    | ToggleController


type alias ModelFn =
    Model -> Model


updateRGBA : Rgba.Fn -> ModelFn
updateRGBA fn m =
    let
        newRGBA =
            fn m.rgba
    in
    { m | rgba = newRGBA, hsla = Rgba.toHSLA newRGBA }


updateHSLA : Hsla.Fn -> ModelFn
updateHSLA fn m =
    let
        newHSLA =
            fn m.hsla
    in
    { m | hsla = newHSLA, rgba = Hsla.toRGBA newHSLA }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( hslaColor, rgbaColor ) =
            ( model.hsla, model.rgba )
    in
    case msg of
        Nop ->
            ( model, Cmd.none )

        ToggleController ->
            ( { model | isControllerCollapsed = not model.isControllerCollapsed }, Cmd.none )

        Red val ->
            ( updateRGBA (\c -> { c | red = val }) model, Cmd.none )

        Green val ->
            ( updateRGBA (\c -> { c | green = val }) model, Cmd.none )

        Blue val ->
            ( updateRGBA (\c -> { c | blue = val }) model, Cmd.none )

        Alpha val ->
            ( model |> updateHSLA (\r -> { r | alpha = val })
            , Cmd.none
            )

        Hue val ->
            ( model |> updateHSLA (\r -> { r | hue = val })
            , Cmd.none
            )

        Saturation val ->
            ( model |> updateHSLA (\r -> { r | saturation = val })
            , Cmd.none
            )

        Lightness val ->
            ( model |> updateHSLA (\r -> { r | lightness = val }), Cmd.none )

        Tick newTime ->
            ( { model | time = newTime, counter = model.counter + 1 }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1000 / 60) Tick



--- View ---


modelColor : Model -> Element.Color
modelColor =
    .rgba >> ElementX.fromRGBA


view : Model -> Html Msg
view model =
    Element.layout
        (fillWH ++ [ fz 1, bc lightGray ])
        (column
            (clipFillWH ++ [])
            [ viewContent model
            , viewController model
            ]
        )


viewContent : Model -> Element msg
viewContent model =
    el
        (scrollFillWH ++ [ p 1 ])
        (el
            [ width fill
            , height (fill |> minRem 40)
            , bc (modelColor model)
            , elevation 3
            ]
            (column (fillWH ++ [ p 1 ])
                [ el [ p 1, centerX, fz 4 ] (text "Color Converter")
                , el fillWH (Element.html SvgView.view)
                ]
            )
        )


viewController : Model -> Element Msg
viewController model =
    column
        [ width fill

        --        , height (shrink |> maxRem 20)
        , fz -2
        , bc (grayscale 0.1)
        , fc (grayscale 0.9)
        , Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
        , Border.shadow
            { offset = ( 0, -2 )
            , size = 0
            , blur = 16
            , color = Element.rgba 0 0 0 0.4
            }

        --  , elevation 24
        ]
        [ el
            [ width fill
            , p -2
            , elevation 4
            , Element.Events.onClick ToggleController
            , Element.pointer
            , bc black
            ]
            (text "Controller")
        , if model.isControllerCollapsed then
            Element.none

          else
            viewColorSliders model
        ]



--- View : Color Slider ---


type alias ColorSliderConfig msg =
    { alt :
        { max : Float
        , min : Float
        , step : Float
        , round : Int
        }
    , labelText : String
    , max : Float
    , min : Float
    , step : Float
    , onChange : Float -> msg
    , value : Float
    }


colorSliderConfig : ColorSliderConfig Msg
colorSliderConfig =
    { value = 0.0
    , labelText = ""
    , onChange = \_ -> Nop
    , min = 0.0
    , max = 1.0
    , step = 0.01
    , alt =
        { min = 0.0
        , max = 255.0
        , step = 1.0
        , round = 0
        }
    }


viewColorSliders model =
    let
        conf =
            colorSliderConfig

        defAlt =
            conf.alt

        hslSliders =
            let
                { hue, saturation, lightness } =
                    model.hsla
            in
            [ { conf
                | value = hue
                , labelText = "hue"
                , onChange = Hue
                , max = 0.99
                , alt =
                    { defAlt
                        | max = 359
                    }
              }
            , { conf
                | value = saturation
                , labelText = "saturation"
                , onChange = Saturation
                , alt =
                    { defAlt
                        | max = 100
                    }
              }
            , { conf
                | value = lightness
                , labelText = "lightness"
                , onChange = Lightness
                , alt =
                    { defAlt
                        | max = 100
                    }
              }
            ]
                |> List.map colorSlider

        rgbSliders =
            let
                { red, green, blue } =
                    model.rgba
            in
            [ { conf | value = red, labelText = "red", onChange = Red }
            , { conf | value = green, labelText = "green", onChange = Green }
            , { conf | value = blue, labelText = "blue", onChange = Blue }
            ]
                |> List.map colorSlider

        row1 =
            row [ width fill, sp -2 ]
                [ column [ sp -4, width fill ] rgbSliders
                , column [ sp -4, width fill ] hslSliders
                ]

        row2 =
            let
                alphaSlider =
                    colorSlider
                        { conf
                            | value = model.rgba.alpha
                            , labelText = "alpha"
                            , onChange = Alpha
                            , alt = { defAlt | min = 0.0, max = 1.0, step = 0.01, round = 2 }
                        }

                previewEl attrs =
                    el [ width fill ]
                        (Rgba.toHexAString model.rgba
                            |> text
                            |> el ([ centerX, p -4 ] ++ attrs)
                        )

                color =
                    ElementX.fromRGBA model.rgba
            in
            row [ width fill, sp -2 ]
                [ alphaSlider
                , row [ width fill, fz -1 ]
                    [ previewEl [ fc color ]
                    , previewEl [ bc color ]
                    ]
                ]
    in
    el
        ([ p -2 ] ++ scrollFillWH)
        (column (fillWH ++ [ sp -6 ]) [ row1, row2 ])


colorSlider : ColorSliderConfig msg -> Element msg
colorSlider { onChange, labelText, value, max, min, step, alt } =
    let
        finalLabelText =
            labelText |> String.left 1 |> String.toUpper
    in
    row [ sp -6, width fill ]
        [ Input.slider
            [ sp -4
            , behindContent
                (el
                    [ width fill
                    , height (px 1)
                    , centerY
                    , Background.color (Element.rgb 0.5 0.5 0.5)
                    , Border.rounded 20
                    ]
                    Element.none
                )
            ]
            { onChange =
                (\v -> v / alt.max)
                    >> clamp alt.min alt.max
                    >> onChange
            , label =
                Input.labelLeft
                    []
                    (el [ alignRight, centerY ] (text finalLabelText))
            , min = alt.min
            , max = alt.max
            , step = Just alt.step
            , value = value * alt.max |> clamp alt.min alt.max
            , thumb =
                thumb
                    [ Element.scale 1
                    , Element.width (Element.px 16)
                    , Element.height (Element.px 16)
                    , Border.rounded 8
                    , Border.width 1
                    , Border.color (Element.rgb 0.5 0.5 0.5)
                    , Background.color (Element.rgb 1 1 1)
                    ]
            }
        , inputNumber
            [ sp -6, p -6 ]
            { onChange =
                (\v -> v / alt.max)
                    >> clamp alt.min alt.max
                    >> onChange
            , min = alt.min
            , max = alt.max
            , step = alt.step
            , round = alt.round
            , label = labelNone
            , value = value * alt.max |> clamp alt.min alt.max
            , placeholder = Nothing
            }
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
