module Todo exposing
    ( Content
    , ListFilter(..)
    , Msg(..)
    , TodoAttrs
    , TodoItem
    , TodoStore
    , content
    , defaultValue
    , isCompleted
    , matchesFilter
    , storeConfig
    )

import BasicsX exposing (Encoder, applyTo, flip, maybeBool)
import JsonCodec as JC exposing (Codec)
import Port
import Store exposing (Item, Store, itemAttrs)


type ListFilter
    = Future
    | Active
    | Completed


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


init : Content -> Bool -> Int -> TodoAttrs
init =
    TodoAttrs


defaultValue : TodoAttrs
defaultValue =
    init "" False 0


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


matchesFilter : Int -> ListFilter -> TodoItem -> Bool
matchesFilter now filter todo =
    let
        completed =
            isCompleted todo

        inFuture =
            isScheduledAfter now todo
    in
    case filter of
        Future ->
            not completed && inFuture

        Active ->
            not completed && not inFuture

        Completed ->
            completed


isCompleted =
    itemAttrs >> .completed


content =
    itemAttrs >> .content


isScheduledAfter now =
    itemAttrs >> .scheduledAt >> (<) now
