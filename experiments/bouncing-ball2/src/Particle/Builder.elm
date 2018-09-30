module Particle.Builder exposing (default, defaultParticle)

import Particle


default =
    { x = 0, y = 0, vm = 0, va = 0, r = 10, mass = 1 }


defaultParticle =
    Particle.new default
