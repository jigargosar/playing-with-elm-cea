module Particle exposing
    ( Particle
    , getAccM
    , getR
    , getXYPair
    , new
    , setAccMA
    , update
    )

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec, r : Float, acc : Vec }


new { x, y, vm, va, r, am, aa } =
    Particle { pos = Vec.newXY x y, vel = Vec.newMA vm va, r = r, acc = Vec.newMA am aa }


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
