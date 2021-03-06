module Main exposing (main)

import Browser
import Color
import Config
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
import Experiments exposing (modelCmdAndThen)
import Generators exposing (boolGenerator, idGenerator, wordsGenerator)
import Hex
import Hsla
import Html exposing (Html, button, col, div, h1, h3, img, input)
import Html.Attributes exposing (class, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Ports
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
import Todo



---- MODEL ----


type alias Flags =
    { config : Config.Model
    , time : Time.Posix
    }


defaultFlags =
    Flags Config.default (Time.millisToPosix 0)


flagsDecoder : D.Decoder Flags
flagsDecoder =
    let
        decodePosixTime =
            D.map Time.millisToPosix D.int
    in
    D.map2 Flags
        (D.field "config" Config.decoder)
        (D.field "time" decodePosixTime)


type alias Model =
    { hsla : Hsla.HSLA
    , rgba : Rgba.RGBA
    , todoCollection : Todo.TodoCollection
    , config : Config.Model
    }


init : E.Value -> ( Model, Cmd Msg )
init flagsValue =
    let
        initialRGBA : Rgba.RGBA
        initialRGBA =
            Rgba.create 1 1 1 1

        flags : Flags
        flags =
            D.decodeValue flagsDecoder flagsValue
                |> Result.mapError (Debug.log "ERROR decoding flags")
                |> Result.withDefault defaultFlags

        model : Model
        model =
            { rgba = initialRGBA
            , hsla = Rgba.toHSLA initialRGBA
            , todoCollection = Dict.empty
            , config = flags.config
            }
    in
    update (Init flags.time) model


getCurrentTodoList : Model -> Todo.TodoList
getCurrentTodoList =
    .todoCollection >> Dict.values


getConfig : Model -> Config.Model
getConfig =
    .config



{- ( model, cache (encodeFlags flags) ) -}
---- UPDATE ----


type Msg
    = Nop
    | Init Time.Posix
    | Red Float
    | Green Float
    | Blue Float
    | Alpha Float
    | Hue Float
    | Saturation Float
    | Lightness Float
    | ToggleConfig
    | PersistConfig
    | Done Todo.Todo Bool
    | AddClicked
    | AddTodos Todo.TodoList


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


setTodoCollectionIn : Model -> Todo.TodoCollection -> Model
setTodoCollectionIn model todoCollection =
    { model | todoCollection = todoCollection }


persistConfigCmd model =
    Ports.store ( "config", model |> getConfig >> Config.encode )


toggleConfigCollapsed model =
    { model | config = Config.toggleConfigCollapsed model.config }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init time ->
            ( model
            , Cmd.batch
                [ persistConfigCmd model
                , Todo.generate AddTodos 10
                ]
            )

        Nop ->
            ( model, Cmd.none )

        Done todo done ->
            ( Todo.updateTodo
                (\t -> { t | done = done })
                todo
                model.todoCollection
                |> setTodoCollectionIn model
            , Cmd.none
            )

        PersistConfig ->
            ( model, persistConfigCmd model )

        ToggleConfig ->
            update PersistConfig (toggleConfigCollapsed model)

        AddTodos todos ->
            ( Todo.addTodos todos model.todoCollection |> setTodoCollectionIn model
            , Cmd.none
            )

        AddClicked ->
            ( model, Todo.generate AddTodos 1 )

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
        , if (getConfig model).isConfigCollapsed then
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
