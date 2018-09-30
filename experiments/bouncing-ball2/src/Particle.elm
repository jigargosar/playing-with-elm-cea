module Particle exposing
    ( Par
    , acc
    , getMass
    , getPos
    , getPosPair
    , getR
    , new
    , update
    , warp
    )

import BasicsX exposing (vec2FromPair)
import Math.Vector2 as V exposing (Vec2)
import Tuple2


type Par
    = Particle { pos : Vec2, vel : Vec2, r : Float, mass : Float }


new { x, y, vm, va, r, mass } =
    Particle
        { pos = V.vec2 x y
        , vel = vec2FromPair (fromPolar ( vm, degrees va ))
        , r = r
        , mass = mass
        }


update (Particle rec) =
    let
        newPos =
            V.add rec.pos rec.vel
    in
    Particle { rec | pos = newPos }


warp ws (Particle rec) =
    let
        r =
            rec.r

        { x, y } =
            rec.pos |> V.toRecord

        ( ww, wh ) =
            BasicsX.vec2ToPair ws

        x1 =
            -ww / 2

        x2 =
            ww / 2

        y1 =
            -wh / 2

        y2 =
            wh / 2

        newX =
            if x > x2 + r then
                x1 - r

            else if x < x1 - r then
                x2 + r

            else
                x

        newY =
            if y > y2 + r then
                y1 - r

            else if y < y1 - r then
                y2 + r

            else
                y
    in
    Particle { rec | pos = V.vec2 newX newY }


getPosPair (Particle { pos }) =
    V.toRecord pos |> (\{ x, y } -> ( x, y ))


getPos (Particle { pos }) =
    pos


getR (Particle { r }) =
    r


getMass (Particle { mass }) =
    mass


toRec (Particle rec) =
    rec


acc vec (Particle rec) =
    Particle { rec | vel = V.add rec.vel vec }
