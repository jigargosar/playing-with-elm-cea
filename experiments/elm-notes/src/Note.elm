module Note exposing (..)

import Random
import Random.Char
import Random.Extra
import Random.List
import Random.String


type alias Note =
    { content : String }


title =
    .content


init content =
    Note content


setContent content note =
    Note content


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


idGenerator : Random.Generator String
idGenerator =
    Random.list 21 idCharGenerator |> Random.map String.fromList
