module ContextStore exposing
    ( Context
    , ContextId
    , ContextName
    , ContextStore
    , Msg
    , addNew
    , archive
    , defaultId
    , defaultName
    , get
    , getNameOrDefaultById
    , list
    , load
    , nameDict
    , setName
    , update
    )

import BasicsX exposing (..)
import Dict exposing (Dict)
import Json.Decode
import JsonCodec as JC exposing (Codec)
import JsonCodecX exposing (Value, decodeValue)
import Log
import Port
import RandomId exposing (withNewId)
import Task
import UpdateReturn exposing (..)


defaultId =
    ""


defaultName =
    "Inbox"


type alias ContextName =
    String


type alias ContextId =
    String


type alias Context =
    { id : ContextId
    , createdAt : Millis
    , modifiedAt : Millis
    , archived : Bool
    , name : ContextName
    }


contextCodec : Codec Context
contextCodec =
    Context
        |> JC.first "id" JC.string .id
        |> JC.next "createdAt" JC.int .createdAt
        |> JC.next "modifiedAt" JC.int .modifiedAt
        |> JC.next "archived" JC.bool .archived
        |> JC.next "name" JC.string .name
        |> JC.end


type alias ContextStore =
    { contextLookup : Dict ContextId Context }


emptyStore : ContextStore
emptyStore =
    ContextStore Dict.empty


contextStoreCodec : Codec ContextStore
contextStoreCodec =
    JC.dict contextCodec
        |> JC.map ContextStore .contextLookup


load : Value -> ( Maybe Log.Line, ContextStore )
load =
    decodeValue contextStoreCodec emptyStore


type Msg
    = NoOp
    | AddNew ContextName
    | AddNewWithNow ContextName Millis
    | AddNewWithNowAndId ContextName Millis ContextId
    | UpsertContextAndCache Context
    | Cache
    | UpdateContext ContextId ContextMsg
    | UpdateContextWithNow ContextId ContextMsg Millis


type ContextMsg
    = SetName ContextName
    | Archive


addNew =
    AddNew


setName id name =
    UpdateContext id (SetName name)


archive id =
    UpdateContext id Archive


update : Msg -> ContextStore -> ( ContextStore, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        AddNew name ->
            ( model, withNowMilli <| AddNewWithNow name )

        AddNewWithNow name now ->
            ( model, withNewId <| AddNewWithNowAndId name now )

        AddNewWithNowAndId name now id ->
            let
                newContext : Context
                newContext =
                    { id = id
                    , createdAt = now
                    , modifiedAt = now
                    , archived = False
                    , name = name
                    }
            in
            update (UpsertContextAndCache newContext) model

        UpsertContextAndCache context ->
            pure { model | contextLookup = Dict.insert context.id context model.contextLookup }
                |> andThenUpdate Cache

        Cache ->
            ( model, Port.cacheContextStore (JC.encoder contextStoreCodec model) )

        UpdateContext id msg ->
            ( model, withNowMilli <| UpdateContextWithNow id msg )

        UpdateContextWithNow id msg now ->
            let
                newMsg =
                    Dict.get id model.contextLookup
                        |> Maybe.andThen (maybeUpdateContext now msg)
                        |> unwrapMaybe NoOp UpsertContextAndCache
            in
            update newMsg model


maybeUpdateContext now msg context =
    let
        updatedContext =
            case msg of
                SetName name ->
                    { context | name = name }

                Archive ->
                    { context | archived = True }
    in
    maybeBool (updatedContext /= context) { updatedContext | modifiedAt = now }


andThenUpdate msg =
    andThen (update msg)


list =
    .contextLookup >> Dict.values


dict =
    .contextLookup


nameDict : ContextStore -> Dict ContextId ContextName
nameDict =
    .contextLookup
        >> Dict.map (\_ c -> c.name)
        >> Dict.insert defaultId defaultName


getNameOrDefaultById : ContextId -> ContextStore -> ContextName
getNameOrDefaultById id =
    get id >> unwrapMaybe defaultName .name


get : ContextId -> ContextStore -> Maybe Context
get id =
    .contextLookup >> Dict.get id
