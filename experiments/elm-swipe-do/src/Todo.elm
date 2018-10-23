module Todo exposing
    ( Content
    , Id
    , State(..)
    , Todo
    , changeStateTo
    , decoder
    , delete
    , encode
    , getContent
    , getState
    , init
    , initWithContent
    , isCompleted
    , setContent
    , stateEq
    )

import Array
import Collection exposing (Millis)
import IdX
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Random
import Schedule exposing (Schedule)


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


getState =
    .state


stringFromState state =
    case state of
        Scheduled ->
            "Scheduled"

        Active ->
            "Active"

        Completed ->
            "Completed"


stateEq state todo =
    state == todo.state


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
    , deleted : Bool
    , state : State
    , schedule : Schedule
    , id : Collection.Id
    , createdAt : Int
    , modifiedAt : Int
    }


encode model =
    E.object
        [ ( "content", E.string model.content )
        , ( "deleted", E.bool model.deleted )
        , ( "state", E.string (stringFromState model.state) )
        , ( "schedule", Schedule.encode model.schedule )
        , ( "id", E.string model.id )
        , ( "createdAt", E.int model.createdAt )
        , ( "modifiedAt", E.int model.modifiedAt )
        ]


decoder : Decoder Model
decoder =
    D.map7 Model
        (D.field "content" D.string)
        (D.field "deleted" D.bool)
        (D.field "state" stateDecoder)
        --        (D.oneOf [ D.field "schedule" Schedule.decoder, D.succeed Schedule.none ])
        (D.field "schedule" Schedule.decoder)
        (D.field "id" D.string)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)


init : Id -> Millis -> Model
init =
    initWithContent ""


initWithContent : Content -> Id -> Millis -> Model
initWithContent content id now =
    { content = content
    , deleted = False
    , state = Active
    , schedule = Schedule.none
    , id = id
    , createdAt = now
    , modifiedAt = now
    }


getContent =
    .content


isCompleted =
    stateEq Completed


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



--markComplete : Millis -> Model -> Model
--markComplete now model =
--    if isCompleted model then
--        model
--
--    else
--        { model | state = Completed, modifiedAt = now }


changeStateTo : State -> Millis -> Model -> Model
changeStateTo newState now model =
    if stateEq newState model then
        model

    else
        { model | state = newState, modifiedAt = now }
