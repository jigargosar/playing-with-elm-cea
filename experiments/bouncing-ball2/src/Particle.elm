module Particle exposing (Particle, newXY, setVel, setVelXY, update, zero)

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec }


newXY x y =
    Particle { pos = Vec.new x y, vel = Vec.zero }


zero =
    newXY 0 0


setVel vel (Particle rec) =
    Particle { rec | vel = vel }


setVelXY x y (Particle rec) =
    Particle { rec | vel = Vec.new x y }


update (Particle rec) =
    Particle { rec | pos = Vec.add rec.pos rec.vel }
