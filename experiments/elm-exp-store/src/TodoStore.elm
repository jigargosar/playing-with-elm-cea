module TodoStore exposing (Content, Todo)

import BasicsX exposing (Encoder, applyTo, flip, maybeBool)
import JsonCodec as JC exposing (Codec)
import Port


type alias Content =
    String


type alias Todo =
    { content : Content
    , completed : Bool
    , scheduledAt : Int
    }
