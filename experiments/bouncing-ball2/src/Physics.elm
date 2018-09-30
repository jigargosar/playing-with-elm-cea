module Physics exposing (Mass, Particle, Position, Radius, Velocity, default)

import Vector as V exposing (Vector)


type Position
    = Position Vector


type Velocity
    = Velocity Vector


type Mass
    = Mass Float


type Radius
    = Radius Float


type Particle
    = Particle Position Velocity Radius Mass


default : Particle
default =
    Particle (Position V.zero) (Velocity V.zero) (Radius 1) (Mass 1)
