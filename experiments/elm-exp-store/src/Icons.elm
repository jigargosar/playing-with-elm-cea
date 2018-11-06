module Icons exposing (Icon, archive, checkCircle, circle, plus, withStyle, withStyleAndAttr)

import Css
import Html.Styled exposing (Html)
import Svg.Styled as Svg exposing (Attribute, Svg, svg)
import Svg.Styled.Attributes exposing (..)



--svgFeatherIcon : String -> List (Svg msg) -> Html msg
--svgFeatherIcon iconName =
--    svg
--        [ class <| "feather-icon feather-icon-" ++ iconName
--        , fill "none"
--        , height "24"
--        , stroke "currentColor"
--        , strokeLinecap "round"
--        , strokeLinejoin "round"
--        , strokeWidth "1"
--        , viewBox "0 0 24 24"
--        , width "24"
--        ]


type alias SvgAttrs =
    { fill : String
    , width : String
    , height : String
    , stroke : String
    , strokeLinecap : String
    , strokeWidth : String
    , viewBox : String
    }


type alias Icon =
    { name : String
    , src : List (Svg Never)

    --    , attrs : SvgAttrs
    }



--defaultAttrs : SvgAttrs
--defaultAttrs =
--    let
--        size =
--            "24"
--    in
--    { fill = "none"
--    , width = size
--    , height = size
--    , stroke = "currentColor"
--    , strokeLinecap = "round"
--    , strokeWidth = "2"
--    , viewBox = [ "0 0", size, size ] |> String.join " "
--    }


create : String -> List (Svg Never) -> Icon
create iconName src =
    Icon iconName src



{- defaultAttrs -}


defaultAttrs =
    [ class "feather-icon"
    , fill "none"
    , height "24"
    , stroke "currentColor"
    , strokeLinecap "round"
    , strokeLinejoin "round"
    , strokeWidth "1.5"
    , viewBox "0 0 24 24"
    , width "24"
    ]


withStyle s =
    withStyleAndAttr s []


withStyleAndAttr : List Css.Style -> List (Attribute msg) -> Icon -> Html msg
withStyleAndAttr styles_ attrs icon =
    Svg.styled Svg.svg
        styles_
        (defaultAttrs
            ++ [ class <| "feather-icon-" ++ icon.name ]
            ++ attrs
        )
        (List.map (Svg.map never) icon.src)



--        (List.map (Svg.map never) icon.src)
--        [ class <| "feather-icon feather-icon-" ++ iconName
--        , fill "none"
--        , height "24"
--        , stroke "currentColor"
--        , strokeLinecap "round"
--        , strokeLinejoin "round"
--        , strokeWidth "1"
--        , viewBox "0 0 24 24"
--        , width "24"
--        ]


archive : Icon
archive =
    create "archive"
        [ Svg.polyline [ points "21 8 21 21 3 21 3 8" ] []
        , Svg.rect [ x "1", y "3", width "22", height "5" ] []
        , Svg.line [ x1 "10", y1 "12", x2 "14", y2 "12" ] []
        ]


plus : Icon
plus =
    create "plus"
        [ Svg.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


checkCircle : Icon
checkCircle =
    create "check-circle"
        [ Svg.path [ d "M22 11.08V12a10 10 0 1 1-5.93-9.14" ] []
        , Svg.polyline [ points "22 4 12 14.01 9 11.01" ] []
        ]


svgFeatherIcon =
    create


circle : Icon
circle =
    svgFeatherIcon "circle"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        ]
