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
    , modifyItemWithId
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


insert : Item attrs -> Model attrs -> ( Item attrs, Model attrs )
insert item model =
    ( item, { model | dict = Dict.insert item.meta.id item model.dict } )


toIdItemPairList : Model attrs -> List ( Id, Item attrs )
toIdItemPairList =
    .dict >> Dict.toList


type Msg attrs
    = NoOp
    | CreateAndInsert attrs
    | CreateAndInsertWithId attrs Id
    | CreateAndInsertWithMeta attrs Meta
    | UpdateModifiedAtOnAttributeChange (Item attrs)
    | ModifiedAtChanged (Item attrs)


type Exit attrs
    = ExitNewInserted ( Item attrs, Model attrs )
    | ExitItemModified ( Item attrs, Model attrs )


createAndInsert =
    CreateAndInsert


modifyItemWithId id updateAttrFn =
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
        >> Maybe.map UpdateModifiedAtOnAttributeChange
        >> Maybe.withDefault NoOp


update : Msg attrs -> Model attrs -> Step (Model attrs) (Msg attrs) (Exit attrs)
update message model =
    case message of
        NoOp ->
            Step.stay

        CreateAndInsert attrs ->
            Step.to model
                |> Step.withCmd (Random.generate (CreateAndInsertWithId attrs) IdX.stringIdGenerator)

        CreateAndInsertWithId attrs id ->
            Step.to model
                |> Step.withCmd
                    (Time.now
                        |> Task.map (Time.posixToMillis >> initMeta id)
                        |> Task.perform (CreateAndInsertWithMeta attrs)
                    )

        CreateAndInsertWithMeta attrs meta ->
            let
                newItem =
                    Item meta attrs

                newId =
                    meta.id
            in
            Step.exit (ExitNewInserted (insert newItem model))

        UpdateModifiedAtOnAttributeChange item ->
            Step.to model
                |> Step.withCmd
                    (Time.now
                        |> Task.map (Time.posixToMillis >> setModifiedAt item)
                        >> Task.perform ModifiedAtChanged
                    )

        ModifiedAtChanged item ->
            Step.exit (ExitItemModified (insert item model))



---- External


type alias Store item =
    Model item



---- Item


type alias Milli =
    Int


type alias Meta =
    { id : Id
    , createdAt : Milli
    , modifiedAt : Milli
    , deleted : Bool
    }


initMeta id now =
    { id = id, createdAt = now, modifiedAt = now, deleted = False }


metaDecoder : Decoder Meta
metaDecoder =
    D.map4 Meta
        (D.field "id" D.string)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "deleted" D.bool)


metaEncoder : Encoder Meta
metaEncoder model =
    E.object
        [ ( "id", E.string model.id )
        , ( "createdAt", E.int model.createdAt )
        , ( "modifiedAt", E.int model.modifiedAt )
        , ( "deleted", E.bool model.deleted )
        ]


type alias Item attrs =
    { meta : Meta
    , attrs : attrs
    }


initItem : attrs -> Milli -> Item attrs
initItem attrs now =
    { meta = { id = "dummyId", createdAt = now, modifiedAt = now, deleted = False }, attrs = attrs }


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
