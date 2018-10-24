module Store exposing
    ( Exit(..)
    , Id
    , Item
    , Msg
    , Store
    , createAndInsert
    , initEmpty
    , insert
    , load
    , overItemAttrs
    , toIdItemPairList
    , update
    )

import BasicsX exposing (Encoder, maybeBool)
import Dict exposing (Dict)
import IdX
import Json.Decode as D exposing (Decoder, decodeValue)
import Json.Encode as E exposing (Value)
import Random exposing (Generator, Seed)
import Step exposing (Step)
import Store.Item as Item exposing (Item)
import Task exposing (Task)
import Time


type alias Id =
    String


type alias CacheName =
    String


type alias Dict_ attrs =
    Dict Id (Item attrs)


type alias Model attrs =
    { dict : Dict_ attrs }


initEmpty : Model attrs
initEmpty =
    init Dict.empty


init : Dict_ attrs -> Model attrs
init dict =
    { dict = dict }


decoder : Decoder attrs -> Decoder (Model attrs)
decoder attrsDecoder =
    D.map init
        (D.dict (Item.decoder attrsDecoder))


encode : Encoder attrs -> Encoder (Model attrs)
encode attrsEncoder model =
    E.object
        [ ( "dict", E.dict identity (Item.encoder attrsEncoder) model.dict )
        ]


load : Decoder attrs -> Value -> Result D.Error (Model attrs)
load =
    decoder >> decodeValue


insert : ( Id, Item attrs ) -> Model attrs -> Model attrs
insert ( id, item ) model =
    { model | dict = Dict.insert id item model.dict }


toIdItemPairList : Model attrs -> List ( Id, Item attrs )
toIdItemPairList =
    .dict >> Dict.toList


newItem : attrs -> Task x (Item attrs)
newItem =
    Item.new


type Msg attrs
    = NoOp
    | CreateAndInsert attrs
    | NewCreated (Item attrs)
    | NewWithIdCreated (Item attrs) Id
    | UpdateModifiedAtOnAttributeChange Id (Item attrs)
    | ModifiedAtChanged Id (Item attrs)


type Exit attrs
    = ExitNewInserted ( ( Id, Item attrs ), Model attrs )
    | ExitItemUpdated ( ( Id, Item attrs ), Model attrs )


createAndInsert =
    CreateAndInsert


overItemAttrs id updateAttrFn =
    .dict
        >> Dict.get id
        >> Maybe.andThen
            (\item ->
                let
                    newAttrs =
                        updateAttrFn item.attrs
                in
                maybeBool (item.attrs /= newAttrs) { item | attrs = newAttrs }
            )
        >> Maybe.map (UpdateModifiedAtOnAttributeChange id)
        >> Maybe.withDefault NoOp


update : Msg attrs -> Model attrs -> Step (Model attrs) (Msg attrs) (Exit attrs)
update message model =
    case message of
        NoOp ->
            Step.stay

        CreateAndInsert attrs ->
            Step.to model |> Step.withCmd (newItem attrs |> Task.perform NewCreated)

        NewCreated item ->
            Step.to model |> Step.withCmd (Random.generate (NewWithIdCreated item) IdX.stringIdGenerator)

        NewWithIdCreated item id ->
            let
                idItemTuple =
                    ( id, item )
            in
            Step.exit (ExitNewInserted ( idItemTuple, insert idItemTuple model ))

        UpdateModifiedAtOnAttributeChange id item ->
            Step.to model
                |> Step.withCmd
                    (Time.now
                        |> Task.map (Time.posixToMillis >> Item.setModifiedAt item)
                        >> Task.perform (ModifiedAtChanged id)
                    )

        ModifiedAtChanged id item ->
            let
                idItemTuple =
                    ( id, item )
            in
            Step.exit (ExitItemUpdated ( idItemTuple, insert idItemTuple model ))



---- External


type alias Store item =
    Model item


type alias Item attrs =
    Item.Item attrs