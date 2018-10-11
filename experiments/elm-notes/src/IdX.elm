module IdX exposing (..)

import Id
import Random


idChars : List Char
idChars =
    let
        alphabets =
            { lowercase = "abcdefghijklmnopqrstuvwxyz"
            , uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            }

        numbers =
            "0123456789"
    in
        alphabets.lowercase ++ alphabets.uppercase ++ numbers ++ "_" |> String.toList


idCharGenerator : Random.Generator Char
idCharGenerator =
    Random.uniform '~' idChars


stringIdGenerator : Random.Generator String
stringIdGenerator =
    Random.list 21 idCharGenerator |> Random.map String.fromList


generator =
    stringIdGenerator |> Random.map (Id.fromString)
