module Ret exposing (Return(..), WithModel(..), WithoutModel(..))


type WithoutModel cmd
    = WithoutModel (List cmd)


type WithModel model cmd
    = WithModel model (List cmd)


type Return model msg
    = Return model (Cmd msg)


with : model -> WithModel model cmd
with model =
    WithModel model []


only : model -> Return model msg
only model =
    Return model Cmd.none


type alias Model =
    {}


type Msg
    = NoOp


type Out
    = ExitOut


update : Msg -> Model -> Return Model Msg
update message model =
    case message of
        NoOp ->
            only model
