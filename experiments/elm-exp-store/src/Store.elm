module Store exposing
    ( Exit(..)
    , Id
    , Item
    , Msg
    , Store
    , createAndInsert
    , initEmpty
    , insert
    , load
    , overItemAttrs
    , toIdItemPairList
    , update
    )

import BasicsX exposing (Encoder, maybeBool)
import Dict exposing (Dict)
import IdX
import Json.Decode as D exposing (Decoder, decodeValue)
import Json.Encode as E exposing (Value)
import Random exposing (Generator, Seed)
import Step exposing (Step)
import Task exposing (Task)
import Time


type alias Id =
    String


type alias CacheName =
    String


type alias Dict_ attrs =
    Dict Id (Item attrs)


type alias Model attrs =
    { dict : Dict_ attrs }


initEmpty : Model attrs
initEmpty =
    init Dict.empty


init : Dict_ attrs -> Model attrs
init dict =
    { dict = dict }


decoder : Decoder attrs -> Decoder (Model attrs)
decoder attrsDecoder =
    D.map init
        (D.dict (itemDecoder attrsDecoder))


encode : Encoder attrs -> Encoder (Model attrs)
encode attrsEncoder model =
    E.object
        [ ( "dict", E.dict identity (itemEncoder attrsEncoder) model.dict )
        ]


load : Decoder attrs -> Value -> Result D.Error (Model attrs)
load =
    decoder >> decodeValue


insert : ( Id, Item attrs ) -> Model attrs -> Model attrs
insert ( id, item ) model =
    { model | dict = Dict.insert id item model.dict }


toIdItemPairList : Model attrs -> List ( Id, Item attrs )
toIdItemPairList =
    .dict >> Dict.toList


type Msg attrs
    = NoOp
    | CreateAndInsert attrs
    | NewCreated (Item attrs)
    | NewWithIdCreated (Item attrs) Id
    | UpdateModifiedAtOnAttributeChange Id (Item attrs)
    | ModifiedAtChanged Id (Item attrs)


type Exit attrs
    = ExitNewInserted ( ( Id, Item attrs ), Model attrs )
    | ExitItemUpdated ( ( Id, Item attrs ), Model attrs )


createAndInsert =
    CreateAndInsert


overItemAttrs id updateAttrFn =
    .dict
        >> Dict.get id
        >> Maybe.andThen
            (\item ->
                let
                    newAttrs =
                        updateAttrFn item.attrs
                in
                maybeBool (item.attrs /= newAttrs) { item | attrs = newAttrs }
            )
        >> Maybe.map (UpdateModifiedAtOnAttributeChange id)
        >> Maybe.withDefault NoOp


update : Msg attrs -> Model attrs -> Step (Model attrs) (Msg attrs) (Exit attrs)
update message model =
    case message of
        NoOp ->
            Step.stay

        CreateAndInsert attrs ->
            Step.to model |> Step.withCmd (newItemTask attrs |> Task.perform NewCreated)

        NewCreated item ->
            Step.to model |> Step.withCmd (Random.generate (NewWithIdCreated item) IdX.stringIdGenerator)

        NewWithIdCreated item id ->
            let
                idItemTuple =
                    ( id, item )
            in
            Step.exit (ExitNewInserted ( idItemTuple, insert idItemTuple model ))

        UpdateModifiedAtOnAttributeChange id item ->
            Step.to model
                |> Step.withCmd
                    (Time.now
                        |> Task.map (Time.posixToMillis >> setModifiedAt item)
                        >> Task.perform (ModifiedAtChanged id)
                    )

        ModifiedAtChanged id item ->
            let
                idItemTuple =
                    ( id, item )
            in
            Step.exit (ExitItemUpdated ( idItemTuple, insert idItemTuple model ))



---- External


type alias Store item =
    Model item



---- Item


type alias Milli =
    Int


type alias Meta =
    { createdAt : Milli
    , modifiedAt : Milli
    , deleted : Bool
    }


metaDecoder : Decoder Meta
metaDecoder =
    D.map3 Meta
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "deleted" D.bool)


metaEncoder : Encoder Meta
metaEncoder model =
    E.object
        [ ( "createdAt", E.int model.createdAt )
        , ( "modifiedAt", E.int model.modifiedAt )
        , ( "deleted", E.bool model.deleted )
        ]


type alias Item attrs =
    { meta : Meta
    , attrs : attrs
    }


initItem : attrs -> Milli -> Item attrs
initItem attrs now =
    { meta = { createdAt = now, modifiedAt = now, deleted = False }, attrs = attrs }


itemDecoder : Decoder attrs -> Decoder (Item attrs)
itemDecoder attrsDecoder =
    D.map2 Item
        (D.field "meta" metaDecoder)
        (D.field "attrs" attrsDecoder)


itemEncoder : Encoder attrs -> Encoder (Item attrs)
itemEncoder attrsEncoder model =
    E.object
        [ ( "meta", metaEncoder model.meta )
        , ( "attrs", attrsEncoder model.attrs )
        ]


newItemTask : attrs -> Task x (Item attrs)
newItemTask attrs =
    Time.now |> Task.map (Time.posixToMillis >> initItem attrs)


setModifiedAt model now =
    let
        meta =
            model.meta
    in
    { model | meta = { meta | modifiedAt = now } }
