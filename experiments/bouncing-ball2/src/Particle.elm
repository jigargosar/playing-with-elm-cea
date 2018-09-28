module Particle exposing (Particle, newXY, posPair, setVelMA, setVelXY, update, zero)

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec }


newXY x y =
    Particle { pos = Vec.newXY x y, vel = Vec.zero }


zero =
    newXY 0 0



--setVel vel (Particle rec) =
--    Particle { rec | vel = vel }


setVelXY x y (Particle rec) =
    Particle { rec | vel = Vec.newXY x y }


setVelMA mag ang (Particle rec) =
    Particle { rec | vel = Vec.newMA mag ang }


update (Particle rec) =
    Particle { rec | pos = Vec.add rec.pos rec.vel }


posPair (Particle { pos }) =
    Vec.toPair pos
