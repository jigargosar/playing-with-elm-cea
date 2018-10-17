module Main exposing (..)

import Browser
import Collection
import Editable
import Exts.Html.Events
import Exts.List
import Exts.Maybe exposing (maybe)
import HotKey exposing (defaultHotKey)
import Html exposing (Html, button, div, h1, img, text, textarea)
import Html.Attributes exposing (attribute, autofocus, class, href, id, src, tabindex, value)
import Browser as B
import Browser.Navigation as Nav
import Browser.Events as B
import Browser.Events as BE
import Browser.Dom as B
import Browser.Dom as BD
import Html as H exposing (Html)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Keyboard.Event
import Keyboard.Key
import Markdown
import Note exposing (Note)
import Pages.EditNote as EditNote
import Ports
import Random
import Task
import Time
import Update.Extra
import Url
import Url.Builder
import Url.Parser as UrlPar exposing ((</>))
import Return3 as R3
import UserInput


-- Routing


type Route
    = NoteList
    | NoteDetail Collection.Id
    | NoteEdit Collection.Id
    | NotFound Url.Url


routeParser : UrlPar.Parser (Route -> a) a
routeParser =
    UrlPar.oneOf
        [ UrlPar.map NoteList UrlPar.top
        , UrlPar.map (NoteDetail) (UrlPar.s "note" </> UrlPar.string)
        , UrlPar.map (NoteEdit) (UrlPar.s "note" </> UrlPar.s "edit" </> UrlPar.string)
        ]


routeToUrlString route =
    let
        absolute =
            Url.Builder.absolute
    in
        case route of
            NoteList ->
                absolute [] []

            NoteDetail id ->
                absolute [ "note", id ] []

            NoteEdit id ->
                absolute [ "note", "edit", id ] []

            NotFound url ->
                Url.toString url


stepUrl url hasNC =
    let
        route =
            UrlPar.parse routeParser url |> Maybe.withDefault (NotFound url)
    in
        case route of
            NoteEdit id ->
                (getNoteById id hasNC)
                    |> Maybe.map createNoteEditPage
                    |> Maybe.withDefault (NotFoundPage "Note Not Found")

            NoteDetail id ->
                (getNoteById id hasNC)
                    |> Maybe.map (\n -> NoteDetailPage n)
                    |> Maybe.withDefault (NotFoundPage "Note Not Found")

            NoteList ->
                NoteListPage

            NotFound _ ->
                NotFoundPage "Oops Something went wrong"



-- Page


type Page
    = EditNotePage EditNote.Model
    | NotFoundPage String
    | NoteListPage
    | NoteDetailPage Note


createNoteEditPage note =
    EditNotePage <| EditNote.init note



--- Auth


type alias UserDetails =
    { uid : String
    , email : String
    , displayName : String
    }


type AuthState
    = Authenticated UserDetails
    | Anon
    | InitialUnknown


userDetailsDecoder : Decoder UserDetails
userDetailsDecoder =
    D.map3 UserDetails
        (D.field "uid" D.string)
        (D.field "email" D.string)
        (D.field "displayName" D.string)


authStateDecoder : Decoder AuthState
authStateDecoder =
    D.oneOf [ D.null Anon, D.map Authenticated userDetailsDecoder ]



-- Notes


type alias NoteCollection =
    Collection.Model Note


currentNoteList : Model -> List Note
currentNoteList =
    .noteCollection
        >> Collection.items
        >> List.filter (.deleted >> not)
        >> List.sortBy (.modifiedAt >> (*) -1)


getNoteById id =
    .noteCollection >> Collection.get id



---- MODEL ----


type alias Flags =
    { now : Int, noteList : E.Value }


type alias Model =
    { noteCollection : NoteCollection
    , authState : AuthState
    , key : Nav.Key
    , url : Url.Url
    , page : Page
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        _ =
            Browser.application

        ( noteCollection, _ ) =
            Random.step (Collection.generator Note.decoder flags.noteList) (Random.initialSeed flags.now)
    in
        ( { noteCollection = noteCollection
          , authState = InitialUnknown
          , key = key
          , url = url
          , page = stepUrl url { noteCollection = noteCollection }
          }
        , Cmd.none
        )



---- UPDATE ----


type alias Millis =
    Int


type alias NoteContent =
    Note.Content


type alias F a =
    a -> a


type Msg
    = NoOp
    | SetNoteCollectionAndPersist NoteCollection
    | NewNoteClicked
    | NewNoteAdded ( Note, NoteCollection )
    | AuthState E.Value
    | SignIn
    | SignOut
    | ReplaceNoteCollection E.Value
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | RouteTo Route
    | PushIfChanged String
    | EditNoteMsg EditNote.Msg


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ Ports.authStateChanged AuthState
        , Ports.notesCollectionChanged ReplaceNoteCollection
        , EditNote.subscriptions |> Sub.map EditNoteMsg
        ]


andThen f ( m1, c1 ) =
    let
        ( m2, c2 ) =
            f m1
    in
        ( m2, Cmd.batch [ c1, c2 ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PushIfChanged urlStr ->
            let
                currentUrlStr =
                    Url.toString model.url
            in
                if urlStr /= currentUrlStr then
                    ( model, Nav.pushUrl model.key urlStr )
                else
                    ( model, Cmd.none )

        RouteTo route ->
            update (PushIfChanged <| routeToUrlString route) model

        UrlRequested urlReq ->
            case urlReq of
                Browser.Internal url ->
                    update (PushIfChanged <| Url.toString url) model

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, page = stepUrl url model }
            , Cmd.none
            )

        EditNoteMsg pageMsg ->
            case model.page of
                EditNotePage pageModel ->
                    EditNote.update pageMsg pageModel
                        |> Tuple.mapBoth
                            (\newPageMode -> { model | page = EditNotePage newPageMode })
                            (Cmd.map EditNoteMsg)

                _ ->
                    ( model, Cmd.none )

        ReplaceNoteCollection encodedValue ->
            model
                |> setNoteCollectionAndPersist
                    (Collection.replace Note.decoder encodedValue model.noteCollection)

        SignIn ->
            ( model, Ports.signIn () )

        SignOut ->
            ( model, Ports.signOut () )

        AuthState encAuthState ->
            let
                newAuthState =
                    D.decodeValue authStateDecoder encAuthState
                        --                        |> Result.mapError (Debug.log "Error: authState")
                        |> Result.withDefault model.authState
            in
                ( { model | authState = newAuthState }, Cmd.none )

        SetNoteCollectionAndPersist newNoteCollection ->
            setNoteCollectionAndPersist newNoteCollection model

        NewNoteAdded ( note, noteCollection ) ->
            setNoteCollectionAndPersist noteCollection model
                |> andThen (update <| RouteTo <| NoteEdit note.id)

        NewNoteClicked ->
            ( model
            , Collection.createAndAdd (Note.init) model.noteCollection
                |> Task.perform NewNoteAdded
            )


setNoteCollectionAndPersist noteCollection model =
    ( { model | noteCollection = noteCollection }
    , Ports.persistNoteCollection <| Collection.encode Note.encode noteCollection
    )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Notes", body = [ htmlView model ] }


htmlView : Model -> Html Msg
htmlView model =
    case model.page of
        EditNotePage pageRec ->
            viewNoteEditPage model pageRec

        NoteDetailPage note ->
            viewNoteDetailPage model note

        NotFoundPage message ->
            viewNotFoundPage model message

        NoteListPage ->
            viewNoteListPage model


bbtn msg title =
    button [ class "ttu", onClick msg ] [ text title ]


viewNotFoundPage model message =
    model |> withDefaultLayout [ h1 [] [ text message ] ]



---- Layout ----


withDefaultLayout viewContent model =
    div [ class "pv3 flex flex-column vh-100 vs3" ]
        [ div [ class "vs3 center w-90" ]
            [ viewHeader model.authState
            ]
        , div [ class "flex-grow-1 flex flex-row justify-center" ]
            [ div [ class "w-90" ] viewContent
            ]
        ]



---- HEADER VIEW ----


viewHeader authState =
    let
        viewAuthState =
            div [ class "flex items-center hs3" ]
                (case authState of
                    InitialUnknown ->
                        [ div [] [ text <| "InitialUnknown" ] ]

                    Anon ->
                        [ div [] [ text <| "SignedOut" ], bbtn SignIn "SignIn" ]

                    Authenticated user ->
                        [ div [] [ text <| "SignedIn: " ++ user.displayName ], bbtn SignOut "SignOut" ]
                )
    in
        div [ class "flex items-center hs3" ]
            [ div [ class "f3 tc" ] [ H.a [ class "link black", href "/" ] [ H.text "Elm Notes" ] ]
            , viewAuthState
            ]



---- NOTE DETAIL PAGE ----


viewNoteDetailPage model note =
    let
        content =
            Note.getContent note
    in
        model
            |> withDefaultLayout
                [ bbtn (RouteTo <| NoteEdit note.id) "Edit"
                , div [ class " pv2 " ] <| Markdown.toHtml Nothing content
                ]



---- NOTE Edit PAGE ----


viewNoteEditPage model pageModel =
    model
        |> withDefaultLayout
            [ textarea
                [ class "pa2 h-100 w-100"
                , value <| EditNote.content pageModel
                , onInput (EditNote.ContentChanged >> EditNoteMsg)
                , onBlur (EditNote.ContentBlurred |> EditNoteMsg)
                ]
                []
            ]



---- NOTE LIST PAGE ----


viewNoteListPage model =
    model
        |> withDefaultLayout
            [ bbtn NewNoteClicked "New"
            , viewNoteList <| currentNoteList model
            ]


viewNoteListDisplayItem note =
    let
        lines =
            Note.getContent note |> String.trim |> String.lines

        firstLine =
            List.head lines |> Maybe.withDefault "<Empty>"

        otherLines =
            List.tail lines |> Maybe.withDefault [] |> String.join " " |> String.slice 0 100

        routeToNoteDetailViewMsg =
            RouteTo <| NoteDetail note.id
    in
        div
            [ onClick routeToNoteDetailViewMsg
            , Exts.Html.Events.onEnter routeToNoteDetailViewMsg
            , class "link black pv2 pointer "
            , tabindex 0
            ]
            [ div [ class "f5" ] [ text firstLine ]
            , div [ class "f6 truncate black-60" ] [ text otherLines ]
            ]


viewNoteList notes =
    let
        viewItem note =
            div [ class "bb b--black-10 pv2" ]
                [ viewNoteListDisplayItem note
                ]
    in
        div [ class "pv1 vs1" ] <| List.map viewItem notes



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
