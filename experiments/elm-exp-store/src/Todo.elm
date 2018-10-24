module Todo exposing (Content, Todo, decoder, init)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type alias Content =
    String


type alias Todo =
    { content : Content
    }


init =
    Todo


decoder : Decoder Todo
decoder =
    D.map Todo (D.field "content" D.string)
