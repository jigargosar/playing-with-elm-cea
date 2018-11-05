module StyleOld exposing (Transform(..), Unit(..), transform)

import Html
import Html.Attributes


type Transform
    = Rotate Unit
    | Translate Unit Unit
    | TranslateY Unit


type Unit
    = Px Float
    | Rem Float
    | Turn Float
    | Zero


transform : List Transform -> Html.Attribute msg
transform =
    let
        stringFromUnit unit =
            case unit of
                Px val ->
                    String.fromFloat val ++ "px"

                Rem val ->
                    String.fromFloat val ++ "rem"

                Turn val ->
                    String.fromFloat val ++ "turn"

                Zero ->
                    "0"

        stringFromTransform t =
            case t of
                Rotate unit ->
                    "rotate(" ++ stringFromUnit unit ++ ")"

                Translate unitX unitY ->
                    "translate(" ++ stringFromUnit unitX ++ "," ++ stringFromUnit unitY ++ ")"

                TranslateY unitY ->
                    "translateY(" ++ stringFromUnit unitY ++ ")"
    in
    List.map stringFromTransform >> String.join " " >> Html.Attributes.style "transform"
