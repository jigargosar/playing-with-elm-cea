module Session exposing (..)

import Collection
import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E
import Note exposing (Note)
import Random


type alias NotesCollection =
    Collection.Model Note


type alias Session =
    { nc : NotesCollection }


generator : E.Value -> Random.Generator Session
generator encodedNC =
    Collection.generator Note.decoder encodedNC
        |> Random.map Session
