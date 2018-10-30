module Mode exposing (Mode(..), Model, Msg(..), OutMsg(..), editContentMode, init, update)

import Store
import Todo exposing (TodoItem)
import UpdateReturn exposing (..)


type Mode
    = Default
    | EditContentMode Store.Id Todo.Content


type alias Model =
    Mode


init : Mode
init =
    Default


type Msg
    = NoOp
    | ContentChangedInStore TodoItem
    | ContentChangedInView Todo.Content
    | EndEditMode


editContentMode todo =
    EditContentMode todo.meta.id todo.attrs.content


type OutMsg
    = TodoInputContentChangedOutMsg Store.Id Todo.Content


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update message model =
    case message of
        NoOp ->
            pure3 model

        ContentChangedInStore updatedTodo ->
            (case model of
                EditContentMode id content ->
                    if updatedTodo.meta.id == id then
                        editContentMode updatedTodo

                    else
                        model

                Default ->
                    model
            )
                |> pure3

        ContentChangedInView newContent ->
            pure3 model
                |> (case model of
                        EditContentMode id _ ->
                            addOutMsg3 (TodoInputContentChangedOutMsg id newContent)

                        Default ->
                            identity
                   )

        EndEditMode ->
            case model of
                EditContentMode id _ ->
                    pure3 Default

                Default ->
                    pure3 model
