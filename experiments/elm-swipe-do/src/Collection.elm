module Collection exposing
    ( Collection
    , Id
    , Ids
    , Millis
    , Model
    , createAndAdd
    , encode
    , generator
    , get
    , getByIdSet
    , idList
    , items
    , replace
    , reset
    , updateIdSetWith
    , updateWith
    )

import Dict exposing (Dict)
import IdX
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Random
import Set exposing (Set)
import Task exposing (Task)
import Time


type alias Id =
    String


type alias Ids =
    Set Id


type alias Millis =
    Int


type alias Model item =
    { dict : Dict String item, seed : Random.Seed }


type alias Collection item =
    Model item


overDict : (Dict String item -> Dict String item) -> Model item -> Model item
overDict updateFn model =
    { model | dict = updateFn model.dict }


reset : Collection item -> Collection item
reset model =
    { model | dict = Dict.empty }


insert : Id -> item -> Model item -> Model item
insert id item =
    overDict <| Dict.insert id item


update : Id -> (item -> item) -> Model item -> Model item
update id f =
    overDict <| Dict.update id (Maybe.map f)


createAndAdd : (Id -> Millis -> item) -> Model item -> Task x ( item, Model item )
createAndAdd initItem model =
    let
        ( id, newSeed ) =
            Random.step IdX.stringIdGenerator model.seed
    in
    createAndAddHelp id initItem { model | seed = newSeed }


createAndAddHelp : Id -> (Id -> Millis -> item) -> Model item -> Task x ( item, Model item )
createAndAddHelp id initItemFn model =
    taskNowMilli
        |> Task.map (initItemFn id >> (\item -> ( item, insert id item model )))


taskNowMilli : Task x Int
taskNowMilli =
    Time.now |> Task.map Time.posixToMillis


updateWith : Id -> (Millis -> item -> item) -> Model item -> Task x (Model item)
updateWith id updateFn model =
    taskNowMilli
        |> Task.map (\nowMilli -> update id (updateFn nowMilli) model)


updateIdSetWith : Set Id -> (Millis -> item -> item) -> Model item -> Task x (Model item)
updateIdSetWith idSet updateFn model =
    taskNowMilli
        |> Task.map (\nowMilli -> idSet |> Set.foldl (\id -> update id (updateFn nowMilli)) model)


encode : (item -> E.Value) -> Model item -> E.Value
encode encodeItem model =
    model.dict |> E.dict identity encodeItem


generator : Decoder item -> E.Value -> Random.Generator (Model item)
generator itemDecoder encodedDict =
    Random.map (Model Dict.empty) Random.independentSeed
        |> Random.map (replace itemDecoder encodedDict)


replace : Decoder item -> E.Value -> Model item -> Model item
replace itemDecoder encodedDict model =
    D.decodeValue (D.dict itemDecoder) encodedDict
        |> Result.map (\dict -> overDict (always dict) model)
        |> Result.mapError identity
        |> Result.withDefault model


items =
    .dict >> Dict.values


idList =
    .dict >> Dict.keys


get id =
    .dict >> Dict.get id


getByIdSet : Set Id -> Collection item -> List item
getByIdSet idSet =
    .dict >> Dict.filter (\id _ -> idSet |> Set.member id) >> Dict.values
