module Config exposing (Model, decoder, default, encode, toggleConfigCollapsed)

import Json.Decode as D
import Json.Encode as E


type alias Model =
    { isConfigCollapsed : Bool }


default =
    Model True


decoder : D.Decoder Model
decoder =
    D.map Model
        (D.field "isConfigCollapsed" D.bool)


encode : Model -> E.Value
encode config =
    E.object
        [ ( "isConfigCollapsed", E.bool config.isConfigCollapsed )
        ]


toggleConfigCollapsed model =
    { model | isConfigCollapsed = not model.isConfigCollapsed }
