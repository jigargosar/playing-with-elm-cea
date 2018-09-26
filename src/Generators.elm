module Generators exposing (boolGenerator, idGenerator)

import Random
import Random.Char
import Random.Extra
import Random.String


boolGenerator : Random.Generator Bool
boolGenerator =
    Random.Extra.bool


idGenerator : Random.Generator String
idGenerator =
    let
        idCharGenerator : Random.Generator Char
        idCharGenerator =
            Random.Extra.frequency
                ( 10, Random.Char.char 48 (48 + 9) )
                [ ( 26 * 2, Random.Char.english ) ]
    in
    Random.String.string 14 idCharGenerator
