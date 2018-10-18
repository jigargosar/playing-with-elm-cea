module Auth exposing (..)

import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E
import Ports


type alias UserDetails =
    { uid : String
    , email : String
    , displayName : String
    , photoUrl : String
    }


type AuthState
    = Authenticated UserDetails
    | Anon
    | InitialUnknown


init =
    InitialUnknown


userDetailsDecoder : Decoder UserDetails
userDetailsDecoder =
    D.map4 UserDetails
        (D.field "uid" D.string)
        (D.field "email" D.string)
        (D.field "displayName" D.string)
        (D.maybe (D.field "photoURL" D.string) |> D.map (Maybe.withDefault ""))


authStateDecoder : Decoder AuthState
authStateDecoder =
    D.oneOf [ D.null Anon, D.map Authenticated userDetailsDecoder ]


type Msg
    = AuthStateChanged E.Value


update msg authState =
    case msg of
        AuthStateChanged encodedValue ->
            ( D.decodeValue authStateDecoder encodedValue
                |> Result.withDefault authState
            , Cmd.none
            )


subscriptions : AuthState -> Sub Msg
subscriptions authState =
    Sub.batch
        [ Ports.authStateChanged AuthStateChanged ]
