module Particle exposing
    ( Particle
    , getAccM
    , getR
    , getXYPair
    , gravitateTo
    , new
    , setAccMA
    , update
    )

import Tuple2
import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec, r : Float, acc : Vec, mass : Float }


new { x, y, vm, va, r, am, aa, mass } =
    Particle
        { pos = Vec.newXY x y
        , vel = Vec.newMA vm va
        , r = r
        , acc = Vec.newMA am aa
        , mass = mass
        }


update (Particle rec) =
    let
        newVel =
            Vec.add rec.vel rec.acc

        newPos =
            Vec.add rec.pos newVel
    in
    Particle { rec | pos = newPos, vel = newVel }


getXYPair (Particle { pos }) =
    Vec.toPair pos


getR (Particle { r }) =
    r


setAccMA mag ang (Particle rec) =
    Particle { rec | acc = Vec.newMA mag ang }


getAccM (Particle { acc }) =
    Vec.toMA acc |> Tuple.first


getMass (Particle { mass }) =
    mass


gravitateTo p2 p1 =
    let
        ( pos1, pos2 ) =
            ( p1, p2 ) |> Tuple2.mapBoth (toRec >> .pos)

        ( dist, ang ) =
            Vec.sub pos1 pos2 |> Vec.toMA

        m =
            getMass p2 * (dist ^ 2)
    in
    setAccMA m ang p1


toRec (Particle rec) =
    rec
