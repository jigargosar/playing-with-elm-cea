module Particle exposing
    ( Particle
    , getR
    , new
    , posPair
    , setVel
    , setVelMA
    , setVelXY
    , update
    )

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec, r : Float, g : Vec }


new x y mag ang r g =
    Particle { pos = Vec.newXY x y, vel = Vec.newMA mag ang, r = r, g = Vec.newXY 0 g }


setVel vel (Particle rec) =
    Particle { rec | vel = vel }


setVelXY x y (Particle rec) =
    Particle { rec | vel = Vec.newXY x y }


setVelMA mag ang (Particle rec) =
    Particle { rec | vel = Vec.newMA mag ang }


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
