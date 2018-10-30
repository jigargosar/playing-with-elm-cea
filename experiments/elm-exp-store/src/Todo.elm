module Todo exposing
    ( Content
    , Msg(..)
    , TodoAttrs
    , TodoItem
    , TodoStore
    , TodoStoreMsg
    , TodoStoreOutMsg
    , content
    , defaultValue
    , initAttrWithContent
    , isCompleted
    , isScheduledAfter
    , isScheduledBefore
    , isScheduledBetween
    , scheduledAt
    , storeConfig
    )

import BasicsX exposing (Encoder, applyTo, flip, maybeBool)
import JsonCodec as JC exposing (Codec)
import Port
import Store exposing (Item, Store, itemAttrs)


type alias Content =
    String


type alias TodoAttrs =
    { content : Content
    , completed : Bool
    , scheduledAt : Int
    }


type alias TodoItem =
    Item TodoAttrs


type alias TodoStore =
    Store TodoAttrs


type alias TodoStoreMsg =
    Store.Msg TodoAttrs


type alias TodoStoreOutMsg =
    Store.OutMsg TodoAttrs


init : Content -> Bool -> Int -> TodoAttrs
init =
    TodoAttrs


defaultValue : TodoAttrs
defaultValue =
    initAttrWithContent ""


initAttrWithContent content_ =
    init content_ False 0


type Msg
    = NoOp
    | SetContent Content
    | MarkCompleted
    | UnmarkCompleted
    | SetScheduledAt Int


storeConfig : Store.Config Msg TodoAttrs
storeConfig =
    let
        codec : Codec TodoAttrs
        codec =
            TodoAttrs
                |> JC.first "content" JC.string .content
                |> JC.option "completed" JC.bool .completed False
                |> JC.option "scheduledAt" JC.int .scheduledAt 0
                |> JC.end

        update : Msg -> TodoAttrs -> Maybe TodoAttrs
        update message model =
            case message of
                NoOp ->
                    Nothing

                SetContent newContent ->
                    maybeBool (model.content /= newContent) { model | content = newContent }

                SetScheduledAt newScheduledAt ->
                    maybeBool (model.scheduledAt /= newScheduledAt) { model | scheduledAt = newScheduledAt }

                MarkCompleted ->
                    maybeBool (not model.completed) { model | completed = True }

                UnmarkCompleted ->
                    maybeBool model.completed { model | completed = False }
    in
    { update = update
    , codec = codec
    , toCacheCmd = Port.cacheTodoStore
    , defaultValue = defaultValue
    }


isCompleted =
    itemAttrs >> .completed


content =
    itemAttrs >> .content


isScheduledAfter now =
    scheduledAt >> (<) now


isScheduledBefore now =
    scheduledAt >> (>) now


isScheduledBetween a b item =
    scheduledAt item <= a && scheduledAt item <= b


scheduledAt =
    itemAttrs >> .scheduledAt
