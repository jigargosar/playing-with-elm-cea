module Color255 exposing (blue, red, white, yellow)

import Color
import Rgba


red =
    Color.toRgba Color.red |> Rgba.to255A


blue =
    Color.toRgba Color.blue |> Rgba.to255A


white =
    Color.toRgba Color.white |> Rgba.to255A


yellow =
    Color.toRgba Color.yellow |> Rgba.to255A
