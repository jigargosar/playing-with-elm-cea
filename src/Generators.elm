module Generators exposing
    ( adjectiveGenerator
    , boolGenerator
    , idGenerator
    , objectNameGenerator
    , wordsGenerator
    )

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


adjectiveGenerator : Random.Generator String
adjectiveGenerator =
    allAdjectives |> Random.Array.sample >> Random.map (Maybe.withDefault "broad")


objectNameGenerator : Random.Generator String
objectNameGenerator =
    objectNames |> Random.Array.sample >> Random.map (Maybe.withDefault "bottle")


wordsGenerator =
    Random.map3 (\w1 w2 w3 -> [ w1, w2, w3 ] |> String.join " ")
        verbGenerator
        adjectiveGenerator
        objectNameGenerator


sampleArrayWithDefault : String -> Array String -> Random.Generator String
sampleArrayWithDefault default =
    Random.Array.sample >> Random.map (Maybe.withDefault default)


verbGenerator : Random.Generator String
verbGenerator =
    sampleArrayWithDefault "heap" verbArray



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


objectNames =
    [ "bottle"
    , "shawl"
    , "toe ring"
    , "street lights"
    , "chapter book"
    , "flag"
    , "pants"
    , "bed"
    , "twister"
    , "perfume"
    , "clothes"
    , "ipod"
    , "bookmark"
    , "clay pot"
    , "thermometer"
    , "hanger"
    , "keyboard"
    , "candle"
    , "button"
    , "monitor"
    , "clamp"
    , "socks"
    , "rubber duck"
    , "pen"
    , "glasses"
    , "cat"
    , "piano"
    , "chalk"
    , "photo album"
    , "flowers"
    , "phone"
    , "tire swing"
    , "lace"
    , "car"
    , "shirt"
    , "bow"
    , "purse"
    , "bag"
    , "nail file"
    , "toothpaste"
    , "television"
    , "paper"
    , "truck"
    , "shoe lace"
    , "glow stick"
    , "sand paper"
    , "scotch tape"
    , "rubber band"
    , "ice cube tray"
    , "mp3 player"
    , "helmet"
    , "teddies"
    , "buckel"
    , "toothbrush"
    , "drill press"
    , "pencil"
    , "drawer"
    , "bracelet"
    , "canvas"
    , "lip gloss"
    , "candy wrapper"
    , "milk"
    , "boom box"
    , "washing machine"
    , "greeting card"
    , "picture frame"
    , "tissue box"
    , "cookie jar"
    , "soda can"
    , "apple"
    , "headphones"
    , "fake flowers"
    , "deodorant"
    , "conditioner"
    , "towel"
    , "stop sign"
    , "air freshener"
    , "sticky note"
    , "tomato"
    , "window"
    , "magnet"
    , "lamp shade"
    , "tree"
    , "toilet"
    , "radio"
    , "face wash"
    , "cork"
    , "lamp"
    , "shampoo"
    , "rusty nail"
    , "nail clippers"
    , "sun glasses"
    , "screw"
    , "paint brush"
    , "mop"
    , "sofa"
    , "glass"
    , "packing peanuts"
    , "spring"
    , "pool stick"
    , "eye liner"
    , "controller"
    , "cell phone"
    , "charger"
    , "credit card"
    , "USB drive"
    , "tooth picks"
    , "fridge"
    , "ring"
    , "knife"
    , "blouse"
    , "cup"
    , "bread"
    , "balloon"
    , "plate"
    , "mirror"
    , "table"
    , "box"
    , "plastic fork"
    , "white out"
    , "bananas"
    , "food"
    , "hair brush"
    , "house"
    , "speakers"
    , "key chain"
    , "soap"
    , "brocolli"
    , "slipper"
    , "doll"
    , "thermostat"
    , "soy sauce packet"
    , "sharpie"
    , "camera"
    , "wallet"
    , "zipper"
    , "bowl"
    , "checkbook"
    , "seat belt"
    , "blanket"
    , "door"
    , "cinder block"
    , "grid paper"
    , "stockings"
    , "couch"
    , "water bottle"
    , "mouse pad"
    , "keys"
    , "vase"
    , "outlet"
    , "hair tie"
    , "money"
    , "CD"
    , "sketch pad"
    , "playing card"
    , "bottle cap"
    , "tv"
    , "pillow"
    , "lotion"
    , "thread"
    , "sandal"
    , "carrots"
    , "floor"
    , "needle"
    , "video games"
    , "rug"
    , "remote"
    , "shovel"
    , "sidewalk"
    , "clock"
    , "sailboat"
    , "chocolate"
    , "puddle"
    , "beef"
    , "watch"
    , "twezzers"
    , "spoon"
    , "fork"
    , "leg warmers"
    , "sponge"
    , "eraser"
    , "newspaper"
    , "book"
    , "chair"
    , "shoes"
    , "wagon"
    , "desk"
    , "model car"
    , "coasters"
    , "computer"
    ]
        |> Array.fromList


verbArray =
    [ "heap"
    , "care"
    , "return"
    , "peck"
    , "brake"
    , "hang"
    , "hunt"
    , "wonder"
    , "remind"
    , "drag"
    , "land"
    , "frighten"
    , "warm"
    , "bury"
    , "prefer"
    , "sip"
    , "visit"
    , "remain"
    , "spill"
    , "occur"
    , "groan"
    , "identify"
    , "cure"
    , "introduce"
    , "carve"
    , "flower"
    , "confuse"
    , "whine"
    , "float"
    , "suck"
    , "blind"
    , "heat"
    , "raise"
    , "kill"
    , "stroke"
    , "carry"
    , "join"
    , "zoom"
    , "sack"
    , "repair"
    , "enjoy"
    , "cover"
    , "rush"
    , "protect"
    , "fool"
    , "burn"
    , "hope"
    , "extend"
    , "offend"
    , "hook"
    , "precede"
    , "mess up"
    , "disagree"
    , "trip"
    , "coach"
    , "damage"
    , "examine"
    , "prepare"
    , "borrow"
    , "strip"
    , "bless"
    , "beam"
    , "produce"
    , "jam"
    , "drain"
    , "gather"
    , "spoil"
    , "scare"
    , "bolt"
    , "remember"
    , "remove"
    , "pretend"
    , "roll"
    , "guess"
    , "glue"
    , "transport"
    , "boil"
    , "hammer"
    , "time"
    , "complete"
    , "pass"
    , "wander"
    , "reply"
    , "save"
    , "relax"
    , "embarrass"
    , "drip"
    , "drown"
    , "recognise"
    , "water"
    , "rinse"
    , "trick"
    , "amuse"
    , "taste"
    , "multiply"
    , "lick"
    , "attempt"
    , "knot"
    , "fasten"
    , "appreciate"
    ]
        |> Array.fromList
