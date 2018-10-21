module Todo exposing (Content, Id, Model)


type alias Id =
    String


type alias Content =
    String


type alias Model =
    { id : Id
    , content : Content
    }
