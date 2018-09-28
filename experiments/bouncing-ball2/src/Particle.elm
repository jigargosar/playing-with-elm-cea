module Particle exposing
    ( Particle
    , fromRec
    , getR
    , newXY
    , posPair
    , setVel
    , setVelMA
    , setVelXY
    , update
    , zero
    )

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec, r : Float }


fromRec { pos, vel, r } =
    Particle { pos = pos, vel = vel, r = r }


newXY x y =
    Particle { pos = Vec.newXY x y, vel = Vec.zero, r = 0 }


zero =
    newXY 0 0


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
