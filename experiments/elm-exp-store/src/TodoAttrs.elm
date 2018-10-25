module TodoAttrs exposing
    ( Content
    , Msg(..)
    , TodoAttrs
    , defaultValue
    , storeConfig
    )

import BasicsX exposing (Encoder, maybeBool)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Port
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


defaultValue =
    init ""


type Msg
    = NoOp
    | SetContent Content


storeConfig : Store.Config Msg TodoAttrs
storeConfig =
    let
        decoder : Decoder TodoAttrs
        decoder =
            D.map TodoAttrs (D.field "content" D.string)

        encoder : Encoder TodoAttrs
        encoder model =
            E.object
                [ ( "content", E.string model.content )
                ]

        update : Msg -> TodoAttrs -> Maybe TodoAttrs
        update message model =
            case message of
                NoOp ->
                    Nothing

                SetContent newContent ->
                    maybeBool (model.content /= newContent) { model | content = newContent }
    in
    { update = update
    , encoder = encoder
    , decoder = decoder
    , toCacheCmd = Port.cacheTodoC
    , defaultValue = defaultValue
    }
