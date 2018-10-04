module Size
    exposing
        ( Size
        , Record
        , Component
        , toRecord
        , toComponent
        , fromComponent
        , fromRecord
        , toRoundIntComponent
        , IntComponent
        )

import Ramda exposing (mapBothWith)
import Round


type alias Record =
    { width : Float, height : Float }


type alias Pair a =
    ( a, a )


type alias Component =
    Pair Float


type alias IntComponent =
    Pair Int


type Size
    = Size Record


fromComponent : Component -> Size
fromComponent ( width, height ) =
    Record width height
        |> Size


fromRecord : Record -> Size
fromRecord =
    Size


toRecord : Size -> Record
toRecord (Size r) =
    r


toComponent : Size -> Component
toComponent =
    toRecord >> \r -> ( r.width, r.height )


toRoundIntComponent : Size -> IntComponent
toRoundIntComponent =
    toComponent >> mapBothWith round
