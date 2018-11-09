module Bouncer exposing (bounce, bounceMaybeMsg, cancel)

import BasicsX exposing (eqs, unwrapMaybe)
import Task
import UpdateReturn exposing (..)


cancel config =
    bounceMaybeMsg config Nothing


bounce config msg =
    bounceMaybeMsg config (Just msg)


bounceMaybeMsg { tagger, emitIfCountMsg } maybeMsg =
    \model ->
        let
            bounceCount =
                model.bounceCount + 1
        in
        ( { model | bounceCount = bounceCount }
        , afterTimeout 0 (emitIfCountMsg bounceCount maybeMsg)
        )
            |> mapCmd tagger
