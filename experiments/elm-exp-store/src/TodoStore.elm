module TodoStore exposing (Content, Id, Todo, TodoStore, load)

import BasicsX exposing (Encoder, Millis, applyTo, flip, maybeBool)
import Dict exposing (Dict)
import JsonCodec as JC exposing (Codec)
import JsonCodecX exposing (Value, decodeValue)
import Log


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
    , scheduledAt : Millis
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
        |> JC.option "scheduledAt" JC.int .scheduledAt 0
        |> JC.end


type alias TodoStore =
    { todoLookup : Dict Id Todo }


emptyStore : TodoStore
emptyStore =
    TodoStore Dict.empty


todoStoreCodec : Codec TodoStore
todoStoreCodec =
    TodoStore
        |> JC.first "todoLookup" (JC.dict todoCodec) .todoLookup
        |> JC.end


load : Value -> ( Maybe Log.Line, TodoStore )
load =
    decodeValue todoStoreCodec emptyStore
