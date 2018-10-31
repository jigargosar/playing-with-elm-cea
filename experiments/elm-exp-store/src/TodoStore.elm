module TodoStore exposing (Content, Id, Todo, TodoStore)

import BasicsX exposing (Encoder, Millis, applyTo, flip, maybeBool)
import Dict exposing (Dict)
import JsonCodec as JC exposing (Codec)
import Port


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
