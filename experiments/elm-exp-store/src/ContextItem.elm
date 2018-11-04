module ContextItem exposing (ContextItem, allContextItems, viewSelectContext)

import ContextStore exposing (ContextId, ContextStore)
import Dict
import Html exposing (Html)
import SelectUI


type alias ContextItem =
    ( String, ContextId )


allContextItems : ContextStore -> List ContextItem
allContextItems =
    ContextStore.list
        >> List.map (\c -> ( c.name, c.id ))
        >> (::) ( ContextStore.defaultName, ContextStore.defaultId )


createContextItemById : ContextStore -> ContextId -> ContextItem
createContextItemById contextStore contextId =
    ( ContextStore.getNameOrDefaultById contextId contextStore, contextId )


viewSelectContext :
    SelectUI.Config msg ContextItem
    -> ContextStore
    -> ContextId
    -> SelectUI.Model
    -> Html msg
viewSelectContext config contextStore currentContextId selectUIModel =
    let
        contextNameLookup =
            ContextStore.nameDict contextStore

        currentContext =
            contextNameLookup
                |> Dict.get currentContextId
    in
    SelectUI.view config
        (Just <| createContextItemById contextStore currentContextId)
        (allContextItems contextStore)
        selectUIModel
