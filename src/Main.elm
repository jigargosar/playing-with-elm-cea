port module Main exposing (ColorSliderConfig, Flags, Model, ModelF, Msg(..), cache, colorSlider, colorSliderConfig, init, main, modelColor, subscriptions, update, updateHSLA, updateRGBA, view, viewColorSliders, viewConfig, viewSampleContent)

import Browser
import Color
import Dict exposing (Dict)
import Element exposing (Element, alignRight, behindContent, centerX, centerY, column, el, fill, height, px, row, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
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
import Generators exposing (boolGenerator, idGenerator, wordsGenerator)
import Hex
import Hsla
import Html exposing (Html, button, col, div, h1, h3, img, input)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Ramda exposing (eqBy, equals, whenEq)
import Random
import Random.Char
import Random.Extra
import Random.String
import Rgba
import Round
import Set
import String.Extra
import Svg as Svg
import Svg.Attributes as SA
import SvgView
import Task
import Time



--- Ports ---


port cache : E.Value -> Cmd msg



---- MODEL ----


type alias Flags =
    { isConfigCollapsed : Bool }


defaultFlags =
    Flags True


encodeFlags : Flags -> E.Value
encodeFlags flags =
    E.object [ ( "isConfigCollapsed", E.bool flags.isConfigCollapsed ) ]


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map Flags
        (D.field "isConfigCollapsed" D.bool)


decodeFlags : E.Value -> Result D.Error Flags
decodeFlags =
    D.decodeValue flagsDecoder


getFlags : Model -> Flags
getFlags { isConfigCollapsed } =
    Flags isConfigCollapsed


type alias Todo =
    { id : String, text : String, done : Bool }


type alias TodoFn =
    Todo -> Todo


type alias TodoList =
    List Todo


type alias TodoCollection =
    Dict String Todo


defaultTodoCollection : TodoCollection
defaultTodoCollection =
    let
        defaultTodoList : TodoList
        defaultTodoList =
            [ Todo "0" "Get Some Milk!" False, Todo "1" "Build Quick Prototype !!" False ]
    in
    defaultTodoList
        |> List.map (\t -> ( t.id, t ))
        |> Dict.fromList


type alias Model =
    { hsla : Hsla.HSLA
    , rgba : Rgba.RGBA
    , isConfigCollapsed : Bool
    , todoCollection : TodoCollection
    }


init : E.Value -> ( Model, Cmd Msg )
init flagsValue =
    let
        initialRGBA : Rgba.RGBA
        initialRGBA =
            Rgba.create 1 1 1 1

        flags : Flags
        flags =
            decodeFlags flagsValue
                |> Result.mapError (Debug.log "ERROR decoding flags")
                |> Result.withDefault defaultFlags

        model : Model
        model =
            { rgba = initialRGBA
            , hsla = Rgba.toHSLA initialRGBA
            , isConfigCollapsed = flags.isConfigCollapsed
            , todoCollection = Dict.empty
            }
    in
    update Cache model
        |> andThenUpdate update AddClicked
        |> andThenUpdate update AddClicked


andThenUpdate updateFn msg ( model, cmd ) =
    let
        ( model2, cmd2 ) =
            updateFn msg model
    in
    ( model2, Cmd.batch [ cmd, cmd2 ] )


getCurrentTodoList : Model -> TodoList
getCurrentTodoList =
    .todoCollection >> Dict.values



{- ( model, cache (encodeFlags flags) ) -}
---- UPDATE ----


type Msg
    = Nop
    | Red Float
    | Green Float
    | Blue Float
    | Alpha Float
    | Hue Float
    | Saturation Float
    | Lightness Float
    | ToggleConfig
    | Cache
    | Done Todo Bool
    | AddClicked
    | AddTodo Todo


type alias ModelF =
    Model -> Model


updateRGBA : Rgba.Fn -> ModelF
updateRGBA fn m =
    let
        newRGBA =
            fn m.rgba
    in
    { m | rgba = newRGBA, hsla = Rgba.toHSLA newRGBA }


updateHSLA : Hsla.Fn -> ModelF
updateHSLA fn m =
    let
        newHSLA =
            fn m.hsla
    in
    { m | hsla = newHSLA, rgba = Hsla.toRGBA newHSLA }


setTodoCollection : TodoCollection -> ModelF
setTodoCollection todoCollection model =
    { model | todoCollection = todoCollection }


updateTodo : TodoFn -> Todo -> ModelF
updateTodo fn todo model =
    model.todoCollection
        |> Dict.update todo.id (Maybe.map fn)
        |> (\tc -> setTodoCollection tc model)


addTodo : Todo -> ModelF
addTodo todo model =
    model.todoCollection
        |> Dict.insert todo.id todo
        |> (\tc -> setTodoCollection tc model)


todoGenerator : Random.Generator Todo
todoGenerator =
    Random.map3 (\id words done -> Todo id (words |> String.Extra.toTitleCase) done)
        idGenerator
        wordsGenerator
        boolGenerator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Done todo done ->
            update Cache (updateTodo (\t -> { t | done = done }) todo model)

        Cache ->
            ( model, cache (model |> getFlags >> encodeFlags) )

        ToggleConfig ->
            update Cache { model | isConfigCollapsed = not model.isConfigCollapsed }

        AddTodo todo ->
            ( addTodo todo model, Cmd.none )

        AddClicked ->
            ( model, Random.generate AddTodo todoGenerator )

        Red val ->
            ( updateRGBA (\c -> { c | red = val }) model, Cmd.none )

        Green val ->
            ( updateRGBA (\c -> { c | green = val }) model, Cmd.none )

        Blue val ->
            ( updateRGBA (\c -> { c | blue = val }) model, Cmd.none )

        Alpha val ->
            ( model |> updateHSLA (\r -> { r | alpha = val }), Cmd.none )

        Hue val ->
            ( model |> updateHSLA (\r -> { r | hue = val }), Cmd.none )

        Saturation val ->
            ( model |> updateHSLA (\r -> { r | saturation = val }), Cmd.none )

        Lightness val ->
            ( model |> updateHSLA (\r -> { r | lightness = val }), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
            [ --
              el [] Element.none
            , viewTodoListPage model
            , viewConfig model
            ]
        )


nop1 =
    \_ -> Nop


viewTodoListPage : Model -> Element Msg
viewTodoListPage model =
    let
        todoItems =
            getCurrentTodoList model
                |> List.map
                    (\todo ->
                        el []
                            (row []
                                [ Input.checkbox []
                                    { onChange = Done todo
                                    , icon = Input.defaultCheckbox
                                    , checked = todo.done
                                    , label = Input.labelRight [] (text todo.text)
                                    }
                                ]
                            )
                    )

        pageTitle =
            el
                [ centerX
                , width (fill |> maxRem 30)
                , fz -1
                , fc (grayscale 0.3)
                ]
                ("Repeat Todo" |> String.toUpper >> text)

        listPaper =
            column
                [ width (fill |> maxRem 30)
                , height fill
                , centerX
                , bc white
                , elevation 3
                , p 1
                ]
                [ column (fillWH ++ [ sp 1 ]) todoItems
                , Input.button [ pXY 1 -6 ] { onPress = Just AddClicked, label = text "Add" }
                ]

        pageContainer =
            column (scrollFillWH ++ [ p 1, sp 1 ])
    in
    pageContainer [ pageTitle, listPaper ]


viewSampleContent : Model -> Element msg
viewSampleContent model =
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


viewConfig : Model -> Element Msg
viewConfig model =
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
            , Element.Events.onClick ToggleConfig
            , Element.pointer
            , bc black
            ]
            (text "Config")
        , if model.isConfigCollapsed then
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
                Input.thumb
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


main : Program E.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
