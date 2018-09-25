module SvgGeometryExample exposing (lineSegment, topLeftFrame, topLeftPoint, view)

import Frame2d
import Geometry.Svg as Svg
import LineSegment2d
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


lineSegment : Svg msg
lineSegment =
    Svg.lineSegment2d
        [ Attributes.stroke "blue"
        , Attributes.strokeWidth "5"
        ]
        (LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates ( 0, 0 )
            , Point2d.fromCoordinates ( 200, 200 )
            )
        )


topLeftPoint =
    Point2d.fromCoordinates ( 0, 300 )


topLeftFrame =
    Frame2d.atPoint topLeftPoint |> Frame2d.reverseY


view =
    Svg.svg
        [ Attributes.width "300"
        , Attributes.height "300"
        ]
        [ Svg.relativeTo topLeftFrame lineSegment ]
