module Mode exposing (Mode(..), Msg(..), init)

import Store
import Todo exposing (TodoItem)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        ContentChangedInStore _ ->
            ( model, Cmd.none )

        ContentChangedInView _ ->
            ( model, Cmd.none )

        EndEditMode ->
            ( model, Cmd.none )
