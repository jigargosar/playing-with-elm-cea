module Particle exposing
    ( Particle
    , fromRec
    , getR
    , posPair
    , setVel
    , setVelMA
    , setVelXY
    , update
    )

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec, r : Float, g : Vec }


fromRec { pos, vel, r } =
    Particle { pos = pos, vel = vel, r = r, g = Vec.zero }


new x y mag ang r g =
    Particle { pos = Vec.newXY x y, vel = Vec.newMA mag ang, r = r, g = Vec.newXY 0 0.1 }


setVel vel (Particle rec) =
    Particle { rec | vel = vel }


setVelXY x y (Particle rec) =
    Particle { rec | vel = Vec.newXY x y }


setVelMA mag ang (Particle rec) =
    Particle { rec | vel = Vec.newMA mag ang }


update (Particle rec) =
    Particle { rec | pos = Vec.add rec.pos rec.vel }


posPair (Particle { pos }) =
    Vec.toPair pos


getR (Particle { r }) =
    r
