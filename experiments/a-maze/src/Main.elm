module Main exposing (main)

import Array2D
import Browser
import Browser.Events
import Coordinate2D as C2 exposing (Coordinate2D, normalizeConnection)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, input, label, text)
import Html.Attributes exposing (checked, class, disabled, type_, value)
import Html.Events exposing (onCheck, onClick, onDoubleClick)
import Html.Lazy
import ISvg
    exposing
        ( iCX
        , iCY
        , iFontSize
        , iHeight
        , iTranslate
        , iTranslateCord
        , iWidth
        , iX
        , iX1
        , iX2
        , iY
        , iY1
        , iY2
        )
import MazeGenerator as MG exposing (MazeGenerator)
import Ramda exposing (equals, flip, ifElse, isEmptyList, ter, unless)
import Random
import Random.Array
import Random.Extra
import Random.List
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (alignmentBaseline, strokeLinecap)
import TypedSvg.Types exposing (AlignmentBaseline(..), StrokeLinecap(..))
import ViewSvgHelpers



---- MODEL ----


type alias Model =
    { seed : Random.Seed
    , mazeGen : MazeGenerator
    , autoStep : Bool
    }


type alias ModelF =
    Model -> Model


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        initialSeed =
            Random.initialSeed now

        ( mazeSeed, modelSeed ) =
            Random.step Random.independentSeed initialSeed
    in
    pure
        { seed = modelSeed
        , mazeGen = MG.init mazeSeed 24 16
        , autoStep = True
        }


isSolved m =
    MG.isSolved m.mazeGen



---- UPDATE ----


type Msg
    = NoOp
    | Step
    | Solve
    | Reset
    | AnimationFrame
    | AutoStep Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        AnimationFrame ->
            m |> ifElse .autoStep (update Step) pure

        Step ->
            unless isSolved (overMG MG.step) m |> pure

        Solve ->
            overMG MG.solve m |> pure

        Reset ->
            overMG MG.reset m |> pure

        AutoStep bool ->
            pure { m | autoStep = bool }


overMG : MG.MazeGeneratorF -> ModelF
overMG fn m =
    { m | mazeGen = fn m.mazeGen }


pure model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view m =
    div [ class "pa3 vs3" ]
        [ div [ class "flex items-end hs3" ] (viewHeaderContent m)
        , div [ class "no-sel" ] [ viewSvg m ]
        ]


viewHeaderContent m =
    let
        solved =
            isSolved m

        stepDisabled =
            solved || m.autoStep
    in
    [ div [ class "f2" ] [ text "A-Maze" ]
    , div [ class "flex items-center hs2 " ]
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Solve, disabled solved ] [ text "Solve" ]
        , label [ class "flex items-center hs2" ]
            [ input [ type_ "checkbox", checked m.autoStep, onCheck AutoStep ] []
            , div [ class "no-sel" ] [ text "Auto Step" ]
            ]
        , button [ onClick Step, disabled stepDisabled ] [ text "Step" ]
        ]
    ]


viewSvg m =
    let
        canvasWidth =
            650

        canvasHeight =
            500
    in
    svg
        [ SA.class "flex center"
        , iWidth (canvasWidth + 10)
        , iHeight (canvasHeight + 10)
        ]
        ([ Svg.g [ SA.opacity "0.2" ] (viewMazeGenerator m.mazeGen)
         , Svg.g [] (viewMaze m.mazeGen)
         ]
            |> ViewSvgHelpers.view
        )


cellSizePx =
    25


viewMaze mg =
    let
        connections : Set MG.Connection
        connections =
            MG.mapConnections normalizeConnection mg
                |> Set.fromList

        isConnected cp =
            Set.member cp connections

        isSouthConnected ( x, y ) =
            isConnected ( ( x, y ), ( x, y + 1 ) )

        isEastConnected ( x, y ) =
            isConnected ( ( x, y ), ( x + 1, y ) )

        offset =
            cellSizePx // 5

        size =
            cellSizePx

        viewCell cord _ =
            let
                ( x, y ) =
                    C2.scale size cord
            in
            Svg.g []
                [ Svg.rect
                    [ iX (x + size - offset)
                    , iY y
                    , iWidth offset
                    , iHeight size
                    , SA.fill "#cd37a9"
                    , SA.fill "none"
                    , SA.fill "#000"
                    , SA.strokeWidth "1"
                    , SA.stroke "#fff"
                    , ter (isEastConnected cord) "0" "1" |> SA.opacity
                    ]
                    []
                , Svg.rect
                    [ iX x
                    , iY (y + size - offset)
                    , iWidth size
                    , iHeight offset
                    , SA.fill "#cd37a9"
                    , SA.fill "none"
                    , SA.fill "#000"
                    , SA.strokeWidth "1"
                    , SA.stroke "#fff"
                    , ter (isSouthConnected cord) "0" "1" |> SA.opacity
                    ]
                    []
                ]
    in
    MG.concatMap viewCell mg


viewMazeGenerator : MazeGenerator -> List (Svg msg)
viewMazeGenerator mg =
    let
        innerOffsetPx =
            cellSizePx // 5

        innerSquareSize =
            cellSizePx - (innerOffsetPx * 2)

        viewMazeGeneratorCell : Coordinate2D -> MG.CellInfo -> Svg msg
        viewMazeGeneratorCell cord { visited, current } =
            Svg.g
                [ cord
                    |> C2.scale cellSizePx
                    >> iTranslateCord
                    >> SA.transform
                ]
                [ Svg.rect
                    [ iX innerOffsetPx
                    , iY innerOffsetPx
                    , iWidth innerSquareSize
                    , iHeight innerSquareSize
                    , SA.fill "#cd37a9"
                    , SA.fill "none"
                    , SA.strokeWidth "3"
                    , SA.stroke "#000"
                    , SA.opacity "0.1"
                    ]
                    []
                , Svg.text_
                    [ iFontSize innerOffsetPx
                    , SA.alignmentBaseline "text-before-edge"
                    ]
                    [ C2.toString cord |> Svg.text ]
                , Svg.g [ SA.opacity "0.5" ]
                    [ Svg.circle
                        [ cellSizePx // 2 |> iCX
                        , cellSizePx // 2 |> iCY
                        , SA.r "5"
                        , ter visited "blue" "none" |> SA.fill
                        ]
                        []
                    , Svg.circle
                        [ cellSizePx // 2 |> iCX
                        , cellSizePx // 2 |> iCY
                        , SA.r "5"
                        , ter current "red" "none" |> SA.fill
                        ]
                        []
                    ]
                ]

        transform =
            C2.scale cellSizePx
                >> C2.translate (cellSizePx // 2)

        viewCellConnection ( from, to ) =
            let
                ( x1, y1 ) =
                    from |> transform

                ( x2, y2 ) =
                    to |> transform

                l =
                    Svg.line
                        [ iX1 x1
                        , iY1 y1
                        , iX2 x2
                        , iY2 y2
                        , SA.strokeWidth "3"
                        , SA.stroke "blue"
                        , SA.opacity "0.3"
                        , strokeLinecap StrokeLinecapRound
                        ]
                        []

                l2 =
                    Svg.line
                        [ iX1 (x1 - 5)
                        , iY1 (y1 - 5)
                        , iX2 x2
                        , iY2 y2
                        , SA.strokeWidth "3"
                        , SA.stroke "blue"
                        , SA.opacity "0.3"
                        , strokeLinecap StrokeLinecapRound
                        ]
                        []

                l3 =
                    Svg.line
                        [ iX1 (x1 + 5)
                        , iY1 (y1 + 5)
                        , iX2 x2
                        , iY2 y2
                        , SA.strokeWidth "3"
                        , SA.stroke "blue"
                        , SA.opacity "0.3"
                        , strokeLinecap StrokeLinecapRound
                        ]
                        []

                viewArrow =
                    Svg.g [] [ l, l2, l3 ]
            in
            l
    in
    MG.mapConnections (normalizeConnection >> viewCellConnection) mg
        ++ MG.concatMap viewMazeGeneratorCell mg



---- PROGRAM ----


subscriptions _ =
    Sub.batch [ Browser.Events.onAnimationFrameDelta (\_ -> AnimationFrame) ]


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.Lazy.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
