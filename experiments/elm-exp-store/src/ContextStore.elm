module ContextStore exposing
    ( Context
    , ContextContent
    , ContextId
    , ContextStore
    , Msg
    , addNew
    , list
    , load
    , markDone
    , setContent
    , unmarkDone
    , update
    )

import BasicsX exposing (..)
import Dict exposing (Dict)
import IdX exposing (withNewId)
import Json.Decode
import JsonCodec as JC exposing (Codec)
import JsonCodecX exposing (Value, decodeValue)
import Log
import Port
import Task
import UpdateReturn exposing (..)


type alias ContextContent =
    String


type alias ContextId =
    String


type alias Context =
    { id : ContextId
    , createdAt : Millis
    , modifiedAt : Millis
    , deleted : Bool
    , content : ContextContent
    , done : Bool
    }


contextCodec : Codec Context
contextCodec =
    Context
        |> JC.first "id" JC.string .id
        |> JC.next "createdAt" JC.int .createdAt
        |> JC.next "modifiedAt" JC.int .modifiedAt
        |> JC.next "deleted" JC.bool .deleted
        |> JC.next "content" JC.string .content
        |> JC.option "done" JC.bool .done False
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
    | AddNew ContextContent
    | AddNewWithNow ContextContent Millis
    | AddNewWithNowAndId ContextContent Millis ContextId
    | UpsertContextAndCache Context
    | Cache
    | UpdateContext ContextId ContextMsg
    | UpdateContextWithNow ContextId ContextMsg Millis


type ContextMsg
    = SetContent ContextContent
    | MarkDone
    | UnmarkDone


addNew =
    AddNew


setContent id content =
    UpdateContext id (SetContent content)


markDone id =
    UpdateContext id MarkDone


unmarkDone id =
    UpdateContext id UnmarkDone


update : Msg -> ContextStore -> ( ContextStore, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        AddNew content ->
            ( model, withNowMilli <| AddNewWithNow content )

        AddNewWithNow content now ->
            ( model, withNewId <| AddNewWithNowAndId content now )

        AddNewWithNowAndId content now id ->
            let
                newContext : Context
                newContext =
                    { id = id
                    , createdAt = now
                    , modifiedAt = now
                    , deleted = False
                    , content = content
                    , done = False
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
                SetContent content ->
                    { context | content = content }

                MarkDone ->
                    { context | done = True }

                UnmarkDone ->
                    { context | done = False }
    in
    maybeBool (updatedContext /= context) { updatedContext | modifiedAt = now }


andThenUpdate msg =
    andThen (update msg)


list =
    .contextLookup >> Dict.values
