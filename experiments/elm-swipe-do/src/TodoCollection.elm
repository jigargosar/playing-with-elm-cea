module TodoCollection exposing
    ( TodoCollection
    , TodoIds
    , addNew
    , cacheCmd
    , generator
    , get
    , items
    , reset
    , setContent
    )

import Collection exposing (Collection)
import Json.Decode as D
import Json.Encode as E
import Port
import Random
import Set exposing (Set)
import Todo exposing (Todo)


type alias TodoIds =
    Set Todo.Id


type alias Model =
    Collection Todo


type alias TodoCollection =
    Model


generator : E.Value -> Random.Generator ( Model, Cmd msg )
generator =
    Collection.generator Todo.decoder


get : Todo.Id -> Model -> Maybe Todo
get =
    Collection.get


items =
    Collection.items


encode =
    Collection.encode Todo.encode


cacheCmd =
    encode >> Port.cacheTodoC


setContent id newContent =
    Collection.updateWith id (Todo.setContent newContent)


addNew =
    Collection.createAndAdd Todo.init


reset =
    Collection.reset
