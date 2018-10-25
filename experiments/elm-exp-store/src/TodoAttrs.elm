module TodoAttrs exposing (Content, TodoAttrs, decoder, init, initEmpty, setContent)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Store exposing (Item)


type alias Content =
    String


type alias TodoAttrs =
    { content : Content
    }


type alias TodoItem =
    Item TodoAttrs


init =
    TodoAttrs


initEmpty =
    init ""


decoder : Decoder TodoAttrs
decoder =
    D.map TodoAttrs (D.field "content" D.string)


setContent : Content -> TodoAttrs -> TodoAttrs
setContent content model =
    { model | content = content }
