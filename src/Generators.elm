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
    objectNames |> Random.Array.sample >> Random.map (Maybe.withDefault "broad")


wordsGenerator =
    Random.map2 (\w1 w2 -> w1 ++ " " ++ w2)
        adjectiveGenerator
        objectNameGenerator



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
