module Main exposing (main)

import Browser
import Color
import Element exposing (Element, alignRight, behindContent, centerX, centerY, column, el, fill, height, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import ElementX
    exposing
        ( bc
        , bcInherit
        , brc
        , clipFillWH
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
    let
        roundedRect : Html msg
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
    in
    el
        (scrollFillWH ++ [ p 1 ])
        (el
            [ width fill
            , bc (modelColor model)
            , elevation 24
            ]
            (column (fillWH ++ [ p 1 ])
                [ el [ p 1, centerX, fz 4 ] (text "Color Converter")
                , el fillWH (Element.html svgView)
                ]
            )
        )


elevation i =
    ("mdc-elevation--z" ++ String.fromInt i)
        |> Html.Attributes.class
        |> Element.htmlAttribute


viewController : Model -> Element Msg
viewController model =
    let
        { hue, saturation, lightness } =
            model.hsla

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
            , color = Element.rgba 0 0 0 0.4
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
            row [ width fill, sp 1 ]
                [ column [ sp 1, width fill ] rgbSliders
                , column [ sp 1, width fill ] hslSliders
                ]

        row2 =
            let
                color =
                    ElementX.fromRGBA model.rgba

                hexA =
                    Rgba.toHexAString model.rgba

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
                        (text hexA |> el ([ centerX, p -4 ] ++ attrs))
            in
            row [ width fill, sp 1 ]
                [ alphaSlider
                , row [ width fill, fz 2 ]
                    [ previewEl [ fc color ]
                    , previewEl [ bc color ]
                    ]
                ]
    in
    el
        ([ p 1 ] ++ scrollFillWH)
        (column (fillWH ++ [ sp 1 ]) [ row1, row2 ])


colorSlider : ColorSliderConfig msg -> Element msg
colorSlider { onChange, labelText, value, max, min, step, alt } =
    let
        finalLabelText =
            labelText |> String.left 1 |> String.toUpper
    in
    row [ sp 1, width fill ]
        [ Input.slider
            [ sp 1
            , behindContent
                (el
                    [ width fill
                    , height (px 2)
                    , centerY
                    , Background.color (Element.rgb 0.5 0.5 0.5)
                    , Border.rounded 2
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
                    (el [ alignRight ] (text finalLabelText))
            , min = alt.min
            , max = alt.max
            , step = Just alt.step
            , value = value * alt.max |> clamp alt.min alt.max
            , thumb =
                Input.defaultThumb
            }
        , inputNumber
            [ sp 0, p -4 ]
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
