module Log exposing (warn)

import Port


warn : String -> List String -> Cmd msg
warn moduleName =
    (::) moduleName >> Port.warn
