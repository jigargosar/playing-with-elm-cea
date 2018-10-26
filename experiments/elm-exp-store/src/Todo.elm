module Todo exposing
    ( Content
    , ListFilter(..)
    , Msg(..)
    , TodoAttrs
    , TodoItem
    , TodoStore
    , defaultValue
    , matchesFilter
    , storeConfig
    )

import BasicsX exposing (Encoder, maybeBool)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Port
import Store exposing (Item, Store)


type ListFilter
    = Future
    | Active
    | Completed


type alias Content =
    String


type alias TodoAttrs =
    { content : Content
    , completed : Bool
    }


type alias TodoItem =
    Item TodoAttrs


type alias TodoStore =
    Store TodoAttrs


init : Content -> Bool -> TodoAttrs
init =
    TodoAttrs


defaultValue : TodoAttrs
defaultValue =
    init "" False


type Msg
    = NoOp
    | SetContent Content
    | MarkCompleted


storeConfig : Store.Config Msg TodoAttrs
storeConfig =
    let
        decoder : Decoder TodoAttrs
        decoder =
            D.map2 TodoAttrs
                (D.field "content" D.string)
                (D.field "completed" D.bool)

        encoder : Encoder TodoAttrs
        encoder model =
            E.object
                [ ( "content", E.string model.content )
                , ( "completed", E.bool model.completed )
                ]

        update : Msg -> TodoAttrs -> Maybe TodoAttrs
        update message model =
            case message of
                NoOp ->
                    Nothing

                SetContent newContent ->
                    maybeBool (model.content /= newContent) { model | content = newContent }

                MarkCompleted ->
                    maybeBool (not model.completed) { model | completed = True }
    in
    { update = update
    , encoder = encoder
    , decoder = decoder
    , toCacheCmd = Port.cacheTodoStore
    , defaultValue = defaultValue
    }


matchesFilter filter todo =
    case filter of
        _ ->
            True
