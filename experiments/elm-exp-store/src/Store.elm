module Store exposing
    ( Config
    , Id
    , Item
    , Msg
    , OutMsg(..)
    , Store
    , insertNew
    , load
    , resetCache
    , toPairs
    , update
    , updateItem
    )

import BasicsX exposing (Encoder, flip, maybeBool, unwrapMaybe)
import Dict exposing (Dict)
import IdX
import Json.Decode exposing (Decoder, decodeValue, errorToString)
import Json.Encode exposing (Value)
import JsonCodec as JC exposing (Codec)
import Log
import Random exposing (Generator, Seed)
import Task exposing (Task)
import Time
import UpdateReturn exposing (addCmd3, addOutMsg3, andThen3, generate3, perform3, pure, pure3)


type alias Id =
    String


type alias Dict_ attrs =
    Dict Id (Item attrs)


type alias Model attrs =
    { dict : Dict_ attrs }


type alias Store item =
    Model item


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


toCacheCmd : Config msg attrs -> Model attrs -> Cmd (Msg attrs)
toCacheCmd config =
    config.toCacheCmd << JC.encoder (storeCodec config.codec)


load : Config msg attrs -> Value -> ( Maybe Log.Line, Model attrs )
load config =
    decodeValue (JC.decoder <| storeCodec config.codec)
        >> (\result ->
                case result of
                    Ok todoStore ->
                        ( Nothing, todoStore )

                    Err error ->
                        ( Just [ errorToString error ], initEmpty )
           )


toPairs : Model attrs -> List ( Id, Item attrs )
toPairs =
    .dict >> Dict.toList


type alias ItemOutMsg attrs =
    Item attrs -> OutMsg attrs


type Msg attrs
    = NoOp
    | Cache
    | ResetCache
    | UpsertItemAndUpdateCacheWithOutMsg (ItemOutMsg attrs) (Item attrs)
    | InsertNew attrs
    | InsertNewWithId attrs Id
    | UpdateModifiedAt (Item attrs)


type OutMsg attrs
    = InsertedOutMsg (Item attrs)
    | ModifiedOutMsg (Item attrs)


insertNew =
    InsertNew


resetCache =
    ResetCache


updateItem : Config msg attrs -> Id -> msg -> Store attrs -> Msg attrs
updateItem config id msg =
    let
        updateItemAttrsMaybe : (attrs -> Maybe attrs) -> Item attrs -> Maybe (Item attrs)
        updateItemAttrsMaybe updateFn model =
            updateFn model.attrs |> Maybe.map (\attrs -> { model | attrs = attrs })
    in
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
            pure3 initEmpty
                |> andThen3 (update config Cache)

        UpsertItemAndUpdateCacheWithOutMsg outMsg item ->
            pure3 { model | dict = Dict.insert item.meta.id item model.dict }
                |> andThen3 (update config Cache)
                |> addOutMsg3 (outMsg item)

        InsertNew attrs ->
            pure3 model
                |> generate3 (InsertNewWithId attrs) IdX.stringIdGenerator

        InsertNewWithId attrs id ->
            pure3 model
                |> perform3
                    (UpsertItemAndUpdateCacheWithOutMsg InsertedOutMsg)
                    (initItemWithNowTask attrs id)

        UpdateModifiedAt item ->
            pure3 model
                |> perform3
                    (UpsertItemAndUpdateCacheWithOutMsg ModifiedOutMsg)
                    (setModifiedAtToNowTask item)



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


type alias Item attrs =
    { meta : Meta
    , attrs : attrs
    }


itemCodec : Codec attrs -> Codec (Item attrs)
itemCodec attrCodec =
    Item
        |> JC.first "meta" metaCodec .meta
        |> JC.next "attrs" attrCodec .attrs
        |> JC.end


initItem attrs meta =
    Item meta attrs


initItemWithNowTask attrs id =
    Time.now
        |> Task.map (Time.posixToMillis >> initMeta id >> initItem attrs)


setItemModifiedAt now model =
    let
        meta =
            model.meta
    in
    { model | meta = { meta | modifiedAt = now } }


setModifiedAtToNowTask model =
    Time.now
        |> Task.map (Time.posixToMillis >> flip setItemModifiedAt model)
