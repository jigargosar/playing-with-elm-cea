module Schedule exposing (Kind(..), Millis, Model, Schedule, decoder, encode, encodeKind, kindDecoder, none)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type alias Millis =
    Int


type alias Model =
    { at : Millis
    , kind : Kind
    }


type alias Schedule =
    Model


type Kind
    = Minutes Int
    | Hours Int
    | LaterToday
    | Tomorrow
    | WeakEnd
    | NextWeek
    | Someday


none =
    Model 0 Someday


encodeKind kind =
    case kind of
        Minutes num ->
            E.object [ ( "Minutes", E.int num ) ]

        Hours num ->
            E.object [ ( "Hours", E.int num ) ]

        LaterToday ->
            E.string "LaterToday"

        Tomorrow ->
            E.string "Tomorrow"

        WeakEnd ->
            E.string "WeakEnd"

        NextWeek ->
            E.string "NextWeek"

        Someday ->
            E.string "Someday"


encode model =
    E.object
        [ ( "at", E.int model.at )
        , ( "kind", encodeKind model.kind )
        ]


kindDecoder =
    let
        kindFromString kindStr =
            case kindStr of
                "LaterToday" ->
                    LaterToday

                "Tomorrow" ->
                    Tomorrow

                "WeakEnd" ->
                    WeakEnd

                "NextWeek" ->
                    NextWeek

                "Someday" ->
                    Someday

                _ ->
                    Someday

        minutesDecoder =
            D.map Minutes (D.field "Minutes" D.int)

        hoursDecoder =
            D.map Hours (D.field "Hours" D.int)
    in
    D.oneOf [ minutesDecoder, hoursDecoder, D.map kindFromString D.string ]


decoder : Decoder Model
decoder =
    D.map2 Model
        (D.field "at" D.int)
        (D.field "kind" kindDecoder)
