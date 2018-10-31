module TodoStore exposing (Content, Id, Todo, TodoStore, load)

import BasicsX exposing (Encoder, Millis, applyTo, flip, maybeBool, nowMilli, withNowMilli)
import Dict exposing (Dict)
import IdX exposing (withNewId)
import Json.Decode
import JsonCodec as JC exposing (Codec)
import JsonCodecX exposing (Value, decodeValue)
import Log
import Task


type alias Content =
    String


type alias Id =
    String


type alias Todo =
    { id : Id
    , createdAt : Millis
    , modifiedAt : Millis
    , deleted : Bool
    , content : Content
    , completed : Bool
    }


todoCodec : Codec Todo
todoCodec =
    Todo
        |> JC.first "id" JC.string .id
        |> JC.next "createdAt" JC.int .createdAt
        |> JC.next "modifiedAt" JC.int .modifiedAt
        |> JC.next "deleted" JC.bool .deleted
        |> JC.next "content" JC.string .content
        |> JC.option "completed" JC.bool .completed False
        |> JC.end


type alias TodoStore =
    { todoLookup : Dict Id Todo }


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
    | AddNew Content
    | AddNewWithNow Content Millis
    | AddNewWithNowAndId Content Millis Id


addNew =
    AddNew


update : Msg -> TodoStore -> ( TodoStore, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

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
                    , completed = False
                    }
            in
            ( model, Cmd.none )
