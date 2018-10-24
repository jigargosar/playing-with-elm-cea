module Todo exposing (Content, TodoAttr, decoder, init, setContent)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Store exposing (Item)


type alias Content =
    String


type alias TodoAttr =
    { content : Content
    }


type alias TodoItem =
    Item TodoAttr


init =
    TodoAttr


decoder : Decoder TodoAttr
decoder =
    D.map TodoAttr (D.field "content" D.string)


setContent : Content -> TodoAttr -> TodoAttr
setContent content model =
    { model | content = content }
