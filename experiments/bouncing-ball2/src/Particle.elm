module Particle exposing
    ( Particle
    , getR
    , new
    , posPair
    , setAccMA
    , update
    )

import Vec exposing (Vec)


type Particle
    = Particle { pos : Vec, vel : Vec, r : Float, acc : Vec }


new x y mag ang r accM accA =
    Particle { pos = Vec.newXY x y, vel = Vec.newMA mag ang, r = r, acc = Vec.newMA accM accA }


update (Particle rec) =
    let
        newVel =
            Vec.add rec.vel rec.acc

        newPos =
            Vec.add rec.pos newVel
    in
    Particle { rec | pos = newPos, vel = newVel }


posPair (Particle { pos }) =
    Vec.toPair pos


getR (Particle { r }) =
    r


setAccMA mag ang (Particle rec) =
    Particle { rec | acc = Vec.newMA mag ang }
