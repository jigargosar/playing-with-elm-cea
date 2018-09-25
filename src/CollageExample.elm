module CollageExample exposing (view)

import Collage exposing (circle, filled, rectangle, uniform)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Color
import Html exposing (Html)
import Rgba


view : Html msg
view =
    let
        circ =
            circle 50
                |> filled (uniform (Color.toRgba Color.red |> Rgba.to255A))

        rect =
            rectangle 200 100
                |> filled (uniform (Color.toRgba Color.blue |> Rgba.to255A))
    in
    rect
        |> at topLeft circ
        |> svg
