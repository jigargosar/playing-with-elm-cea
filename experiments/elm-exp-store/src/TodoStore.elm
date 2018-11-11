module TodoStore exposing
    ( Msg
    , Todo
    , TodoContent
    , TodoId
    , TodoStore
    , addNew
    , delete
    , list
    , load
    , markDone
    , setContent
    , setContextId
    , unmarkDone
    , update
    )

import BasicsX exposing (..)
import ContextStore exposing (ContextId)
import Dict exposing (Dict)
import Json.Decode
import JsonCodec as JC exposing (Codec)
import JsonCodecX exposing (Value, decodeValue)
import Log
import Port
import RandomId exposing (withNewId)
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
    , contextId : ContextId
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
        |> JC.option "contextId" JC.string .contextId ContextStore.defaultId
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
    | AddNew TodoContent ContextId
    | AddNewWithNow TodoContent ContextId Millis
    | AddNewWithNowAndId TodoContent ContextId Millis TodoId
    | UpsertTodoAndCache Todo
    | Cache
    | UpdateTodo TodoId TodoMsg
    | UpdateTodoWithNow TodoId TodoMsg Millis


type TodoMsg
    = SetContent TodoContent
    | SetContextId ContextId
    | MarkDone
    | UnmarkDone
    | Delete


addNew =
    AddNew


setContent id content =
    UpdateTodo id (SetContent content)


setContextId id contextId =
    UpdateTodo id (SetContextId contextId)


markDone id =
    UpdateTodo id MarkDone


unmarkDone id =
    UpdateTodo id UnmarkDone


delete id =
    UpdateTodo id Delete


update : Msg -> TodoStore -> ( TodoStore, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        AddNew content contextId ->
            ( model, withNowMilli <| AddNewWithNow content contextId )

        AddNewWithNow content contextId now ->
            ( model, withNewId <| AddNewWithNowAndId content contextId now )

        AddNewWithNowAndId content contextId now id ->
            let
                newTodo : Todo
                newTodo =
                    { id = id
                    , createdAt = now
                    , modifiedAt = now
                    , deleted = False
                    , content = content
                    , done = False
                    , contextId = contextId
                    }
            in
            update (UpsertTodoAndCache newTodo) model

        UpsertTodoAndCache todo ->
            pure { model | todoLookup = Dict.insert todo.id todo model.todoLookup }
                |> andThenUpdate Cache

        Cache ->
            ( model, Port.cacheTodoStore (JC.encoder todoStoreCodec model) )

        UpdateTodo id msg ->
            ( model, withNowMilli <| UpdateTodoWithNow id msg )

        UpdateTodoWithNow id msg now ->
            let
                newMsg =
                    Dict.get id model.todoLookup
                        |> Maybe.andThen (maybeUpdateTodo now msg)
                        |> unwrapMaybe NoOp UpsertTodoAndCache
            in
            update newMsg model


maybeUpdateTodo now msg todo =
    let
        updatedTodo =
            case msg of
                SetContent content ->
                    { todo | content = content }

                MarkDone ->
                    { todo | done = True }

                UnmarkDone ->
                    { todo | done = False }

                Delete ->
                    { todo | deleted = True }

                SetContextId contextId ->
                    { todo | contextId = contextId }
    in
    maybeBool (updatedTodo /= todo) { updatedTodo | modifiedAt = now }


andThenUpdate msg =
    andThen (update msg)


list =
    .todoLookup >> Dict.values
