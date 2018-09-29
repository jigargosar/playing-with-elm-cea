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
    Vec.getM acc


getMass (Particle { mass }) =
    mass


gravitateTo p2 p1 =
    let
        angleTo (Particle p2R) (Particle p1R) =
            let
                ( dx, dy ) =
                    Vec.sub p1R.pos p2R.pos |> Vec.toPair
            in
            atan2 dy dx

        distanceTo (Particle p2R) (Particle p1R) =
            Vec.sub p1R.pos p2R.pos |> Vec.getM

        dist =
            distanceTo p2 p1

        m =
            getMass p2 * (dist ^ 2)
    in
    setAccMA m (angleTo p2 p1) p1
