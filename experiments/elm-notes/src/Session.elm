module Session exposing (..)

import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


type alias Session =
    { authState : AuthState }


empty =
    { authState = InitialUnknown }


type alias UserDetails =
    { uid : String
    , email : String
    , displayName : String
    }


type AuthState
    = Authenticated UserDetails
    | Anon
    | InitialUnknown


userDetailsDecoder : Decoder UserDetails
userDetailsDecoder =
    D.map3 UserDetails
        (D.field "uid" D.string)
        (D.field "email" D.string)
        (D.field "displayName" D.string)


authStateDecoder : Decoder AuthState
authStateDecoder =
    D.oneOf [ D.null Anon, D.map Authenticated userDetailsDecoder ]


type Msg
    = AuthStateChanged E.Value


update msg model =
    case msg of
        AuthStateChanged encodedValue ->
            ( { model
                | authState =
                    D.decodeValue authStateDecoder encodedValue
                        |> Result.withDefault model.authState
              }
            , Cmd.none
            )
