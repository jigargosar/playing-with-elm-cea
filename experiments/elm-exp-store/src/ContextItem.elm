module ContextItem exposing (ContextItem, allContextItems, viewSelectContext)

import BasicsX exposing (..)
import ContextStore exposing (ContextId, ContextStore)
import Dict exposing (Dict)
import Html exposing (Html)
import SelectUI


type alias ContextItem =
    ( String, ContextId )


viewSelectContext :
    SelectUI.Config msg ContextItem
    -> ContextStore
    -> ContextId
    -> SelectUI.Model
    -> Html msg
viewSelectContext config contextStore currentContextId selectUIModel =
    let
        contextNameLookup : Dict String String
        contextNameLookup =
            ContextStore.nameDict contextStore

        currentContextItem : ContextItem
        currentContextItem =
            contextNameLookup
                |> Dict.get currentContextId
                |> unwrapMaybe ( ContextStore.defaultName, ContextStore.defaultId )
                    (\name -> ( name, currentContextId ))

        allContextItems =
            contextNameLookup
                |> Dict.toList
                |> List.map (\k v -> ( v, k ))
    in
    SelectUI.view config
        (Just <| currentContextItem)
        allContextItems
        selectUIModel
