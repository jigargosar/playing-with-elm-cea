module Particle exposing
    ( Particle
    , getAccM
    , getPosPair
    , getR
    , gravitateTo
    , new
    , setAccMA
    , update
    )

import Math.Vector2 as V exposing (Vec2)
import Tuple2


type Particle
    = Particle { pos : Vec2, vel : Vec2, r : Float, acc : Vec2, mass : Float }


new { x, y, vm, va, r, am, aa, mass } =
    Particle
        { pos = V.vec2 x y
        , vel = vec2FromPair (fromPolar ( vm, degrees va ))
        , r = r
        , acc = vec2FromPair (fromPolar ( am, degrees aa ))
        , mass = mass
        }


update (Particle rec) =
    let
        newVel =
            V.add rec.vel rec.acc

        newPos =
            V.add rec.pos newVel
    in
    Particle { rec | pos = newPos, vel = newVel }


getPosPair (Particle { pos }) =
    V.toRecord pos |> (\{ x, y } -> ( x, y ))


getR (Particle { r }) =
    r


setAccMA mag ang (Particle rec) =
    Particle { rec | acc = vec2FromPair (fromPolar ( mag, degrees ang )) }


getAccM (Particle { acc }) =
    V.length acc


getMass (Particle { mass }) =
    mass


gravitateTo p2 p1 =
    let
        ( pos1, pos2 ) =
            ( p1, p2 ) |> Tuple2.mapBoth (toRec >> .pos)

        ( _, ang ) =
            V.sub pos1 pos2 |> V.toRecord |> (\{ x, y } -> toPolar ( x, y ))

        diffV =
            V.sub pos1 pos2

        m =
            getMass p2 * V.distanceSquared pos1 pos2
    in
    setAccMA m ang p1


toRec (Particle rec) =
    rec


vec2FromPair ( x, y ) =
    V.vec2 x y
