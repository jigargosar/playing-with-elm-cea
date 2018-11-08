module Bouncer exposing (bounce)

import UpdateReturn exposing (..)


bounce toMsg emitIfCountMsg maybeBounceMsg =
    andThen
        (\model ->
            let
                bounceCount =
                    model.bounceCount + 1
            in
            ( { model | bounceCount = bounceCount }
            , afterTimeout 0 (emitIfCountMsg bounceCount maybeBounceMsg)
            )
                |> mapCmd toMsg
        )
