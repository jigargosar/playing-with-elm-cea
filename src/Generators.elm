module Generators exposing (boolGenerator, idGenerator)

import Array exposing (Array)
import Random
import Random.Array
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


generateAdjective : Random.Generator String
generateAdjective =
    allAdjectives |> Random.Array.sample >> Random.map (Maybe.withDefault "")



--- References https://www.learnenglish.de/grammar/adjectivecommon.html ---


allAdjectives =
    adjectives.shape
        ++ adjectives.size
        ++ adjectives.sound
        |> Array.fromList


adjectives =
    { shape =
        [ "broad"
        , "crooked"
        , "curved"
        , "deep"
        , "even"
        , "flat"
        , "hilly"
        , "jagged"
        , "round"
        , "shallow"
        , "square"
        , "steep"
        , "straight"
        , "thick"
        , "thin"
        , "triangular"
        , "uneven"
        ]
    , size =
        [ "average"
        , "big"
        , "fat"
        , "gigantic"
        , "huge"
        , "large"
        , "little"
        , "long"
        , "massive"
        , "medium"
        , "miniature"
        , "narrow"
        , "petite"
        , "short"
        , "skinny"
        , "small"
        , "tall"
        , "tiny"
        , "wide"
        ]
    , sound =
        [ "cooing"
        , "deafening"
        , "faint"
        , "harsh"
        , "high-pitched"
        , "hissing"
        , "hushed"
        , "husky"
        , "loud"
        , "melodic"
        , "moaning"
        , "mute"
        , "noisy"
        , "purring"
        , "quiet"
        , "raspy"
        , "screeching"
        , "shrill"
        , "silent"
        , "soft"
        , "squeaky"
        , "squealing"
        , "thundering"
        , "voiceless"
        , "whispering"
        ]
    }
