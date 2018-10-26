module Store exposing
    ( Config
    , Id
    , Item
    , Msg
    , OutMsg(..)
    , Store
    , createAndInsert
    , initEmpty
    , insert
    , load
    , modifyItemWithId
    , resetCache
    , toIdItemPairList
    , update
    , updateItem
    )

import BasicsX exposing (Encoder, flip, maybeBool, unwrapMaybe)
import Dict exposing (Dict)
import IdX
import Json.Decode as D exposing (Decoder, decodeValue)
import Json.Encode as E exposing (Value)
import JsonCodec as JC exposing (Codec)
import Log
import Random exposing (Generator, Seed)
import Task exposing (Task)
import Time
import UpdateReturn exposing (addCmd3, addOutMsg3, andThen3, perform3, pure, pure3)


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


storeCodec : Codec attrs -> Codec (Model attrs)
storeCodec attrsCodec =
    Model
        |> JC.first "dict" (JC.dict <| itemCodec attrsCodec) .dict
        |> JC.end


storeEncoder : Codec attrs -> Encoder (Model attrs)
storeEncoder attrsCodec model =
    E.object
        [ ( "dict", E.dict identity (itemEncoder <| JC.encoder attrsCodec) model.dict )
        ]


toCacheCmd : Config msg attrs -> Model attrs -> Cmd (Msg attrs)
toCacheCmd config =
    config.toCacheCmd << storeEncoder config.codec


load : Config msg attrs -> Value -> ( Maybe Log.Line, Model attrs )
load config =
    decodeValue (JC.decoder <| storeCodec config.codec)
        >> (\result ->
                case result of
                    Ok todoStore ->
                        ( Nothing, todoStore )

                    Err error ->
                        ( Just [ D.errorToString error ], initEmpty )
           )


insert : Item attrs -> Model attrs -> Model attrs
insert item model =
    { model | dict = Dict.insert item.meta.id item model.dict }


toIdItemPairList : Model attrs -> List ( Id, Item attrs )
toIdItemPairList =
    .dict >> Dict.toList


type alias ItemOutMsg attrs =
    Item attrs -> OutMsg attrs


type Msg attrs
    = NoOp
    | Cache
    | ResetCache
    | InsertItemAndUpdateCacheWithOutMsg (ItemOutMsg attrs) (Item attrs)
    | CreateAndInsert attrs
    | CreateAndInsertWithId attrs Id
    | CreateAndInsertWithMeta attrs Meta
    | UpdateModifiedAt (Item attrs)


type OutMsg attrs
    = InsertedOutMsg (Item attrs)
    | ModifiedOutMsg (Item attrs)


createAndInsert =
    CreateAndInsert


resetCache =
    ResetCache


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
        >> Maybe.map UpdateModifiedAt
        >> Maybe.withDefault NoOp


updateItemAttrsMaybe : (attrs -> Maybe attrs) -> Item attrs -> Maybe (Item attrs)
updateItemAttrsMaybe updateFn model =
    updateFn model.attrs |> Maybe.map (\attrs -> { model | attrs = attrs })


updateItem : Config msg attrs -> Id -> msg -> Store attrs -> Msg attrs
updateItem config id msg =
    .dict
        >> Dict.get id
        >> Maybe.andThen (updateItemAttrsMaybe <| config.update msg)
        >> unwrapMaybe NoOp UpdateModifiedAt


type alias Config msg attrs =
    { update : msg -> attrs -> Maybe attrs
    , codec : Codec attrs
    , toCacheCmd : Value -> Cmd (Msg attrs)
    , defaultValue : attrs
    }


update : Config msg attrs -> Msg attrs -> Model attrs -> ( Model attrs, Cmd (Msg attrs), List (OutMsg attrs) )
update config message model =
    case message of
        NoOp ->
            pure3 model

        Cache ->
            ( model, toCacheCmd config model, [] )

        ResetCache ->
            ( initEmpty, toCacheCmd config initEmpty, [] )

        InsertItemAndUpdateCacheWithOutMsg outMsg item ->
            pure3 (insert item model)
                |> andThen3 (update config Cache)
                |> addOutMsg3 (outMsg item)

        CreateAndInsert attrs ->
            ( model, Random.generate (CreateAndInsertWithId attrs) IdX.stringIdGenerator, [] )

        CreateAndInsertWithId attrs id ->
            ( model
            , Time.now
                |> Task.map (Time.posixToMillis >> initMeta id)
                |> Task.perform (CreateAndInsertWithMeta attrs)
            , []
            )

        CreateAndInsertWithMeta attrs meta ->
            let
                newItem =
                    Item meta attrs

                newModel =
                    insert newItem model
            in
            ( newModel, toCacheCmd config newModel, [ InsertedOutMsg newItem ] )

        UpdateModifiedAt item ->
            pure3 model
                |> perform3 (InsertItemAndUpdateCacheWithOutMsg ModifiedOutMsg)
                    (setModifiedAtToNowTask item)



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


metaCodec : Codec Meta
metaCodec =
    Meta
        |> JC.first "id" JC.string .id
        |> JC.next "createdAt" JC.int .createdAt
        |> JC.next "modifiedAt" JC.int .modifiedAt
        |> JC.next "deleted" JC.bool .deleted
        |> JC.end


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


itemCodec : Codec attrs -> Codec (Item attrs)
itemCodec attrCodec =
    Item
        |> JC.first "meta" metaCodec .meta
        |> JC.next "attrs" attrCodec .attrs
        |> JC.end


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


setModifiedAt now model =
    let
        meta =
            model.meta
    in
    { model | meta = { meta | modifiedAt = now } }


setModifiedAtToNowTask model =
    Time.now
        |> Task.map (Time.posixToMillis >> flip setModifiedAt model)
