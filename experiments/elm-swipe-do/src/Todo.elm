module Todo exposing
    ( Content
    , Id
    , Todo
    , decoder
    , delete
    , encode
    , getContent
    , init
    , initWithContent
    , isDone
    , setContent
    , setDone
    )

import Collection exposing (Millis)
import IdX
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Random


type alias Id =
    Collection.Id


type alias Content =
    String


type alias Todo =
    Model


type State
    = Scheduled
    | Active
    | Completed


stringFromState state =
    case state of
        Scheduled ->
            "Scheduled"

        Active ->
            "Active"

        Completed ->
            "Completed"


stateDecoder : Decoder State
stateDecoder =
    D.map stateFromString D.string


stateFromString stateString =
    case stateString of
        "Scheduled" ->
            Scheduled

        "Active" ->
            Active

        "Completed" ->
            Completed

        _ ->
            Active


type alias Model =
    { content : Content
    , done : Bool
    , deleted : Bool
    , state : State
    , id : Collection.Id
    , createdAt : Int
    , modifiedAt : Int
    }


encode model =
    E.object
        [ ( "content", E.string model.content )
        , ( "done", E.bool model.done )
        , ( "deleted", E.bool model.deleted )
        , ( "state", E.string (stringFromState model.state) )
        , ( "id", E.string model.id )
        , ( "createdAt", E.int model.createdAt )
        , ( "modifiedAt", E.int model.modifiedAt )
        ]


decoder : Decoder Model
decoder =
    D.map7 Model
        (D.field "content" D.string)
        (D.field "done" D.bool)
        (D.field "deleted" D.bool)
        (D.maybe (D.field "state" stateDecoder) |> D.map (Maybe.withDefault Active))
        (D.field "id" D.string)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)


init : Id -> Millis -> Model
init =
    initWithContent ""


initWithContent : Content -> Id -> Millis -> Model
initWithContent content id now =
    { content = content
    , done = False
    , deleted = False
    , state = Active
    , id = id
    , createdAt = now
    , modifiedAt = now
    }


getContent =
    .content


isDone =
    .done


setContent : Content -> Collection.Millis -> Model -> Model
setContent content now model =
    if content == model.content then
        model

    else
        { model | content = content, modifiedAt = now }


delete : Millis -> Model -> Model
delete now model =
    if model.deleted then
        model

    else
        { model | deleted = True, modifiedAt = now }


setDone : Bool -> Millis -> Model -> Model
setDone done now model =
    if done == model.done then
        model

    else
        { model | done = done, modifiedAt = now }
