module TodoCollection exposing (TodoCollection, generator)

import Collection exposing (Collection)
import Json.Decode as D
import Json.Encode as E
import Random
import Todo exposing (Todo)


type alias Model =
    Collection Todo


type alias TodoCollection =
    Model


generator : E.Value -> Random.Generator Model
generator enc =
    Collection.generator Todo.decoder enc
