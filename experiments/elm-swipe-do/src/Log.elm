module Log exposing (Messages, Result, warn)

import Port


type alias Messages =
    List String


type alias Result a =
    Result.Result Messages a


warn : String -> Messages -> Cmd msg
warn moduleName =
    (::) moduleName >> Port.warn
