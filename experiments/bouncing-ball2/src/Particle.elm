module Particle exposing (Particle, newXY, zero)

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec }


newXY x y =
    Particle { pos = Vec.new x y, vel = Vec.zero }


zero =
    newXY 0 0
