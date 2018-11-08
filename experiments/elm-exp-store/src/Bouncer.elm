module Bouncer exposing (bounce, cancel, emitIfBounceCount)

import BasicsX exposing (eqs, unwrapMaybe)
import Task
import UpdateReturn exposing (..)


cancel config =
    bounceMaybeMsg config Nothing


bounce config msg =
    bounceMaybeMsg config (Just msg)


bounceMaybeMsg { toMsg, emitIfCountMsg } maybeMsg =
    andThen
        (\model ->
            let
                bounceCount =
                    model.bounceCount + 1
            in
            ( { model | bounceCount = bounceCount }
            , afterTimeout 0 (emitIfCountMsg bounceCount maybeMsg)
            )
                |> mapCmd toMsg
        )


emitIfBounceCount { toMsg } count maybeMsg =
    andMapWhen (.bounceCount >> eqs count)
        (maybeAddTaggedMsg toMsg maybeMsg
            >> mapModel (\model -> { model | bounceCount = 0 })
        )
