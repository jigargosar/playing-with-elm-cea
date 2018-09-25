module CollageExample exposing (view)

import Collage exposing (circle, defaultLineStyle, dot, filled, line, rectangle, rotate, shift, styled, thick, traced, transparent, uniform)
import Collage.Layout exposing (at, bottom, bottomLeft, bottomRight, center, debug, distances, impose, place, right, stack, topLeft)
import Collage.Render exposing (svg)
import Color255 exposing (blue, red, white, yellow)
import Html exposing (Html)
import Rgba


outlineNone =
    { defaultLineStyle | thickness = 0 }


view : Float -> Html msg
view deg =
    let
        wheel =
            circle 25
                |> styled ( uniform white, defaultLineStyle )

        ln =
            line 100
                |> traced (dot 10 (uniform yellow))

        carBody =
            rectangle 200 100
                |> styled ( uniform blue, outlineNone )

        carBodyTransparent =
            rectangle 200 100
                |> styled ( transparent, outlineNone )

        w1 =
            carBodyTransparent
                |> at bottomRight wheel

        w2 =
            carBodyTransparent
                |> at bottomLeft wheel
    in
    stack
        [ shift ( -50, -50 ) wheel
        , shift ( 50, -50 ) wheel
        , shift ( 50, 50 ) wheel
        , shift ( -50, 50 ) wheel
        , carBody
        ]
        |> rotate (degrees deg)
        |> debug
        |> svg
