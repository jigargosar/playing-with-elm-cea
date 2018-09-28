module Particle exposing
    ( Particle
    , getA
    , getR
    , new
    , posPair
    , update
    )

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec, r : Float, g : Vec }


new x y mag ang r g =
    Particle { pos = Vec.newXY x y, vel = Vec.newMA mag ang, r = r, g = Vec.newXY 0 g }


update (Particle rec) =
    let
        newVel =
            Vec.add rec.vel rec.g

        newPos =
            Vec.add rec.pos newVel
    in
    Particle { rec | pos = newPos, vel = newVel }


posPair (Particle { pos }) =
    Vec.toPair pos


getR (Particle { r }) =
    r


getA (Particle { r }) =
    r
