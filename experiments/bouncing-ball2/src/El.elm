module El exposing (btn, globalStyle, hel)

import Element exposing (el, html, layout, padding, paddingXY, rgba, row, spacing, text)
import Element.Background as Bkg
import Element.Border as Bdr
import Element.Events
import Element.Font exposing (typeface)
import Element.Input


hel ela n na ne =
    el ela (n na ne |> html)


globalStyle =
    [ Element.Font.size 16
    , Element.Font.family
        [ typeface "-apple-system"
        , typeface "BlinkMacSystemFont"
        , typeface "avenir next"
        , typeface "avenir"
        , typeface "helvetica neue"
        , typeface "helvetica"
        , typeface "ubuntu"
        , typeface "roboto"
        , typeface "noto"
        , typeface "segoe ui"
        , typeface "arial"
        , typeface "sans-serif"
        ]
    , Element.Font.family []
    ]


btn : List (Element.Attribute msg) -> msg -> String -> Element.Element msg
btn al msg labelText =
    Element.Input.button
        ([ paddingXY 6 2
         , Bdr.rounded 0
         , Bdr.width 0
         , Bdr.solid
         , Bdr.shadow
            { offset = Tuple.pair 0 0
            , size = 1
            , blur = 1
            , color = rgba 0 0 0 0.2
            }
         , Bdr.color (rgba 0 0 0 0)
         , Bkg.color (rgba 0 0 0 0)
         ]
            ++ al
        )
        { onPress = Just msg, label = el [] (text labelText) }
