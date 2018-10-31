module TodoStore exposing (Content, Todo)

import BasicsX exposing (Encoder, Millis, applyTo, flip, maybeBool)
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
    , scheduledAt : Int
    }
