module Todo exposing (Content, Id, Todo, delete, init, setContent, setDone)

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


type alias Model =
    { content : Content
    , done : Bool
    , deleted : Bool
    , createdAt : Int
    , modifiedAt : Int
    , id : Collection.Id
    }


encode model =
    E.object
        [ ( "id", E.string model.id )
        , ( "content", E.string model.content )
        , ( "done", E.bool model.deleted )
        , ( "deleted", E.bool model.deleted )
        , ( "createdAt", E.int model.createdAt )
        , ( "modifiedAt", E.int model.modifiedAt )
        ]


decoder : Decoder Model
decoder =
    D.map6 Model
        (D.field "content" D.string)
        (D.field "done" D.bool)
        (D.field "deleted" D.bool)
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "id" D.string)


init =
    initWithContent ""


initWithContent content id now =
    { content = content, done = False, deleted = False, id = id, createdAt = now, modifiedAt = now }


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
