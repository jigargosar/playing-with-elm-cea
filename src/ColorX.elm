module ColorX exposing (CH255)


type CH255
    = CH255 Int


type Alpha
    = Alpha Float


type RGBA
    = RGBA CH255 CH255 CH255 Alpha
