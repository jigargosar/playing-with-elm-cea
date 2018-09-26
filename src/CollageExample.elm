module CollageExample exposing (view)

import Collage exposing (circle, defaultLineStyle, dot, filled, line, rectangle, rotate, shift, shiftX, styled, thick, traced, transparent, uniform)
import Collage.Layout exposing (at, base, bottom, bottomLeft, bottomRight, center, debug, distances, impose, left, place, right, spacer, stack, topLeft)
import Collage.Render exposing (svg)
import Color255 exposing (blue, red, white, yellow)
import Html exposing (Html)
import Rgba
import Round


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
    spacer 300 300
        |> at right
            (stack
                [ shift ( -50, -50 ) wheel
                , shift ( 50, -50 ) wheel
                , shift ( 50, 50 ) wheel
                , shift ( -50, 50 ) wheel
                , carBody
                ]
                |> shiftX (Round.truncate deg |> modBy 300 |> toFloat |> negate)
                |> rotate (degrees deg)
            )
--        |> debug
        |> svg
