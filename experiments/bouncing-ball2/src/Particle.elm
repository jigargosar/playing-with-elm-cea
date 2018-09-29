module Particle exposing
    ( Particle
    , acc
    , getPosPair
    , getR
    , new
    , update
    )

import BasicsX exposing (vec2FromPair)
import Math.Vector2 as V exposing (Vec2)
import Tuple2


type Particle
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


getPosPair (Particle { pos }) =
    V.toRecord pos |> (\{ x, y } -> ( x, y ))


getR (Particle { r }) =
    r


getMass (Particle { mass }) =
    mass


toRec (Particle rec) =
    rec


acc vec (Particle rec) =
    Particle { rec | vel = V.add rec.vel vec }
