module Collection exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import IdX
import Json.Decode exposing (Decoder)
import Json.Encode
import Random
import Task exposing (Task)
import Time
import Json.Decode as D
import Json.Encode as E


type alias Id =
    String


type alias Millis =
    Int


type alias Model item =
    { dict : Dict String item, seed : Random.Seed }


overDict : (Dict String item -> Dict String item) -> Model item -> Model item
overDict updateFn model =
    { model | dict = updateFn model.dict }


insert : Id -> item -> Model item -> Model item
insert id item =
    overDict <| Dict.insert id item


update : Id -> (item -> item) -> Model item -> Model item
update id f =
    overDict <| Dict.update id (Maybe.map f)


insertWith : (Id -> Millis -> item) -> Model item -> Task x (Model item)
insertWith initItem model =
    let
        ( id, newSeed ) =
            Random.step IdX.stringIdGenerator model.seed
    in
        insertWithHelp id initItem ({ model | seed = newSeed })


insertWithHelp : Id -> (Id -> Millis -> item) -> Model item -> Task x (Model item)
insertWithHelp id initItemFn model =
    taskNowMilli
        |> Task.map (initItemFn id >> \item -> insert id item model)


taskNowMilli : Task x Int
taskNowMilli =
    Time.now |> Task.map Time.posixToMillis


updateWith : Id -> (Millis -> item -> item) -> Model item -> Task x (Model item)
updateWith id updateFn model =
    taskNowMilli
        |> Task.map (\nowMilli -> update id (updateFn nowMilli) model)


encode : (item -> E.Value) -> Model item -> E.Value
encode encodeItem model =
    model.dict |> E.dict identity encodeItem


generator : Decoder item -> E.Value -> Random.Generator (Model item)
generator itemDecoder encodedDict =
    let
        dict =
            D.decodeValue (D.dict itemDecoder) encodedDict
                |> Result.mapError (identity)
                |> Result.withDefault Dict.empty
    in
        Random.map (Model dict) Random.independentSeed
