module TodoStore exposing (Msg, Todo, TodoContent, TodoId, TodoStore, list, load, update)

import BasicsX exposing (Encoder, Millis, applyTo, flip, maybeBool, nowMilli, withNowMilli)
import Dict exposing (Dict)
import IdX exposing (withNewId)
import Json.Decode
import JsonCodec as JC exposing (Codec)
import JsonCodecX exposing (Value, decodeValue)
import Log
import Port
import Task
import UpdateReturn exposing (..)


type alias TodoContent =
    String


type alias TodoId =
    String


type alias Todo =
    { id : TodoId
    , createdAt : Millis
    , modifiedAt : Millis
    , deleted : Bool
    , content : TodoContent
    , done : Bool
    }


todoCodec : Codec Todo
todoCodec =
    Todo
        |> JC.first "id" JC.string .id
        |> JC.next "createdAt" JC.int .createdAt
        |> JC.next "modifiedAt" JC.int .modifiedAt
        |> JC.next "deleted" JC.bool .deleted
        |> JC.next "content" JC.string .content
        |> JC.option "done" JC.bool .done False
        |> JC.end


type alias TodoStore =
    { todoLookup : Dict TodoId Todo }


emptyStore : TodoStore
emptyStore =
    TodoStore Dict.empty


todoStoreCodec : Codec TodoStore
todoStoreCodec =
    JC.dict todoCodec
        |> JC.map TodoStore .todoLookup


load : Value -> ( Maybe Log.Line, TodoStore )
load =
    decodeValue todoStoreCodec emptyStore


type Msg
    = NoOp
    | AddNew TodoContent
    | AddNewWithNow TodoContent Millis
    | AddNewWithNowAndId TodoContent Millis TodoId
    | UpsertTodoAndCache Todo
    | Cache


addNew =
    AddNew


update : Msg -> TodoStore -> ( TodoStore, Cmd Msg )
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
                newTodo : Todo
                newTodo =
                    { id = id
                    , createdAt = now
                    , modifiedAt = now
                    , deleted = False
                    , content = content
                    , done = False
                    }
            in
            update (UpsertTodoAndCache newTodo) model

        UpsertTodoAndCache todo ->
            pure { model | todoLookup = Dict.insert todo.id todo model.todoLookup }
                |> andThenUpdate Cache

        Cache ->
            ( model, Port.cacheTodoStore (JC.encoder todoStoreCodec model) )


andThenUpdate msg =
    andThen (update msg)


list =
    .todoLookup >> Dict.values
