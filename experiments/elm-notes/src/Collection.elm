module Collection exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import IdX
import Random
import Task exposing (Task)
import Time


type alias Id =
    String


type alias Millis =
    Int


type alias Model item =
    { dict : Dict String item, seed : Random.Seed }


type alias F a =
    a -> a


type alias ModelF item =
    F (Model item)


getDict =
    .dict


setDict dict model =
    { model | dict = dict }


overDict f model =
    setDict (f (getDict model)) model


insert : Id -> item -> ModelF item
insert id item =
    overDict <| Dict.insert id item


insertIn : Model item -> Id -> item -> Model item
insertIn model id item =
    insert id item model


update : Id -> F item -> ModelF item
update id f =
    overDict <| Dict.update id (Maybe.map f)


insertWith : (Id -> Millis -> item) -> Model item -> Task x (Model item)
insertWith initItem model =
    let
        ( newId, newSeed ) =
            Random.step IdX.stringIdGenerator model.seed

        newModel =
            { model | seed = newSeed }
    in
        Task.map2 initItem (Task.succeed newId) (Time.now |> Task.map Time.posixToMillis)
            |> Task.map (insertIn newModel newId)


taskNowMilli : Task x Int
taskNowMilli =
    Time.now |> Task.map Time.posixToMillis


foo : Cmd Int
foo =
    Task.perform identity taskNowMilli
