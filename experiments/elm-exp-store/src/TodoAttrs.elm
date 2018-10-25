module TodoAttrs exposing (Content, TodoAttrs, decoder, init, initEmpty, setContent)

import BasicsX exposing (maybeBool)
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


type Msg
    = NoOp
    | SetContent Content


update : Msg -> TodoAttrs -> Maybe TodoAttrs
update message model =
    case message of
        NoOp ->
            Nothing

        SetContent newContent ->
            maybeBool (model.content /= newContent) { model | content = newContent }
