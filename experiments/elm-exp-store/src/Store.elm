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
    , loadWithDefaultEmpty
    , modifyItemWithId
    , toIdItemPairList
    , update
    , updateItem
    )

import BasicsX exposing (Encoder, maybeBool, unwrapMaybe)
import Dict exposing (Dict)
import IdX
import Json.Decode as D exposing (Decoder, decodeValue)
import Json.Encode as E exposing (Value)
import Random exposing (Generator, Seed)
import Task exposing (Task)
import Time
import Update3
import UpdateReturn exposing (addOutMsg, pure)


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


load : Config msg attrs -> Value -> Result D.Error (Model attrs)
load config =
    decodeValue (decoder config.decoder)


loadWithDefaultEmpty config =
    load config >> Result.withDefault initEmpty


insert : Item attrs -> Model attrs -> Model attrs
insert item model =
    { model | dict = Dict.insert item.meta.id item model.dict }


toIdItemPairList : Model attrs -> List ( Id, Item attrs )
toIdItemPairList =
    .dict >> Dict.toList


type Msg attrs
    = NoOp
    | CreateAndInsert attrs
    | CreateAndInsertWithId attrs Id
    | CreateAndInsertWithMeta attrs Meta
    | InsertModified (Item attrs)
    | InsertModifiedWithNow (Item attrs) Milli


type OutMsg attrs
    = NoOutMsg
    | InsertedOutMsg (Item attrs)
    | ModifiedOutMsg (Item attrs)


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
        >> Maybe.map InsertModified
        >> Maybe.withDefault NoOp


updateItemAttrsMaybe : (attrs -> Maybe attrs) -> Item attrs -> Maybe (Item attrs)
updateItemAttrsMaybe updateFn model =
    updateFn model.attrs |> Maybe.map (\attrs -> { model | attrs = attrs })


updateItem : Config msg attrs -> Id -> msg -> Store attrs -> Msg attrs
updateItem config id msg =
    .dict
        >> Dict.get id
        >> Maybe.andThen (updateItemAttrsMaybe <| config.update msg)
        >> unwrapMaybe NoOp InsertModified


type alias Config msg attrs =
    { update : msg -> attrs -> Maybe attrs
    , encoder : Encoder attrs
    , decoder : Decoder attrs
    , toCacheCmd : Value -> Cmd (Msg attrs)
    , defaultValue : attrs
    }


update : Config msg attrs -> Msg attrs -> Model attrs -> ( Model attrs, Cmd (Msg attrs), OutMsg attrs )
update config message model =
    case message of
        NoOp ->
            pure model |> withNoOutMsg

        CreateAndInsert attrs ->
            ( model, Random.generate (CreateAndInsertWithId attrs) IdX.stringIdGenerator )
                |> withNoOutMsg

        CreateAndInsertWithId attrs id ->
            ( model
            , Time.now
                |> Task.map (Time.posixToMillis >> initMeta id)
                |> Task.perform (CreateAndInsertWithMeta attrs)
            )
                |> withNoOutMsg

        CreateAndInsertWithMeta attrs meta ->
            let
                newItem =
                    Item meta attrs

                newModel =
                    insert newItem model
            in
            ( newModel, config.toCacheCmd <| encode config.encoder newModel )
                |> addOutMsg (InsertedOutMsg newItem)

        InsertModified item ->
            ( model
            , Time.now
                |> Task.map Time.posixToMillis
                |> Task.perform (InsertModifiedWithNow item)
            , NoOutMsg
            )

        InsertModifiedWithNow item now ->
            let
                newItem =
                    setModifiedAt now item

                newModel =
                    insert newItem model
            in
            ( newModel, config.toCacheCmd <| encode config.encoder newModel )
                |> addOutMsg (ModifiedOutMsg newItem)


withNoOutMsg =
    Update3.addOutMsg NoOutMsg



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


setModifiedAt now model =
    let
        meta =
            model.meta
    in
    { model | meta = { meta | modifiedAt = now } }
