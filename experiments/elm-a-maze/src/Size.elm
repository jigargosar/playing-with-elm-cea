module Size
    exposing
        ( Size
        , Record
        , Component
        , toRecord
        , toComponent
        , fromComponent
        , fromRecord
        )


type alias Record =
    { width : Float, height : Float }


type alias Component =
    ( Float, Float )


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
