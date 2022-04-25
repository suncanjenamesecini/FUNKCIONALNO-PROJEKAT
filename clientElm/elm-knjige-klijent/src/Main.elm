module Main exposing (..)

import Browser
import Browser.Events
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html
import Http exposing (Error(..), emptyBody, header, jsonBody, stringBody)
import Json.Decode as JD
import Json.Encode as JE
import Svg as S
import Svg.Attributes as SA


type alias Model =
    { bookTitle : String
    , bookThumbnail : String
    , bookPages : Int -- Int  --Maybe Int maybe.... PROMIJENITI NA MAYBE INT
    , bookLink : String
    , bookPublisher : String
    , bookLanguageId : Int --PROMIJENITI NA MAYBE INT
    , bookId : Int --PROMIJENITI NA MAYBE INT
    , poruka : String
    , results : List Book
    , resultBook : Maybe Book --MOZDA I NE MORA MAYBE JER SIGURNO DOBIJAMO BOOK KAO REZULTAT AKO NEMA GRESKE, ALI INIT MODEL PRAVI PROBLEM BEZ TOGA....
    , errorMessage : Maybe String
    , loading : Bool
    }


type alias Book =
    { title : String
    , thumbnail : Maybe String
    , link : String
    , pages : Maybe Int
    , publisher : Maybe String
    }


type Msg
    = MsgGetBooks
    | MsgGetBooksElm
    | MsgGetBooksHaskell
    | MsgGetBooksElixir
    | MsgGetBooksFsharp
    | MsgGetBooksClojure
    | MsgGetBooksOcaml
    | MsgGetBooksScala
    | MsgGetBooksRacket
    | MsgGetBooksScheme
    | MsgGetBooksLisp
    | MsgGetBooksML
    | MsgGetBooksAPL
    | MsgGetBooksMiranda
    | MsgGetBooksAgda
    | MsgGetBooksErlang
    | MsgGetBooksLargest
    | MsgGotResults (Result Http.Error (List Book))
    | MsgSuccesfulPost (Result Http.Error ()) --(Result Http.Error String) --TODO DODAJ S
      --| GotText (Result Http.Error String)
    | MsgInputTitleField String
    | MsgInputThumbnailField String
    | MsgInputPagesFieldAsString String
    | MsgInputLinkField String
    | MsgInputPublisherField String
    | MsgInputLanguageIdFieldAsString String
    | MsgAddBook
    | MsgDeleteBook
    | MsgShowBook
    | MsgInputIdFieldAsString String
    | MsgGotResult (Result Http.Error (Maybe Book))
    | MsgSuccessfulDelete (Result Http.Error ())



--    | MsgKeyPressed String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, cmdSearchAll )


initModel : Model
initModel =
    { bookTitle = ""
    , bookThumbnail = ""
    , bookPages = 0 --Maybe Int maybe.... mora 0 difoltno
    , bookLink = ""
    , bookPublisher = ""
    , bookLanguageId = 1 --Mora 1 difoltno
    , bookId = 1
    , poruka = ""
    , results = []
    , resultBook = Nothing
    , errorMessage = Nothing
    , loading = False
    }


view : Model -> Html.Html Msg
view model =
    viewLayout model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgInputTitleField newTextInput ->
            ( { model | bookTitle = newTextInput }, Cmd.none )

        MsgInputThumbnailField newThumbnail ->
            ( { model | bookThumbnail = newThumbnail }, Cmd.none )

        MsgInputPagesFieldAsString newPages ->
            ( { model | bookPages = Maybe.withDefault 0 (String.toInt newPages) }, Cmd.none )

        --JOS JE BOLJE DA SE RAZLOZI NA SLUCAJEVE I POSEBNO OBRADE UMJESTO Maybe.withDefault
        MsgInputLinkField newLink ->
            ( { model | bookLink = newLink }, Cmd.none )

        MsgInputPublisherField newPublisher ->
            ( { model | bookPublisher = newPublisher }, Cmd.none )

        MsgInputLanguageIdFieldAsString newLanguageId ->
            ( { model | bookLanguageId = Maybe.withDefault 0 (String.toInt newLanguageId) }, Cmd.none )

        --JOS JE BOLJE DA SE RAZLOZI NA SLUCAJEVE I POSEBNO OBRADE UMJESTO Maybe.withDefault
        MsgAddBook ->
            updateAddBook model

        MsgDeleteBook ->
            updateDeleteBook model

        MsgShowBook ->
            updateShowBook model

        MsgGetBooks ->
            updateSve model

        MsgGetBooksElm ->
            updateElm model

        MsgGetBooksHaskell ->
            updateHaskell model

        MsgGetBooksElixir ->
            updateElixir model

        MsgGetBooksFsharp ->
            updateFsharp model

        MsgGetBooksClojure ->
            updateClojure model

        MsgGetBooksOcaml ->
            updateOcaml model

        MsgGetBooksScala ->
            updateScala model

        MsgGetBooksRacket ->
            updateRacket model

        MsgGetBooksScheme ->
            updateScheme model

        MsgGetBooksLisp ->
            updateLisp model

        MsgGetBooksML ->
            updateML model

        MsgGetBooksAPL ->
            updateAPL model

        MsgGetBooksMiranda ->
            updateMiranda model

        MsgGetBooksAgda ->
            updateAgda model

        MsgGetBooksErlang ->
            updateErlang model

        MsgGetBooksLargest ->
            updateLargest model

        MsgInputIdFieldAsString newId ->
            ( { model | bookId = Maybe.withDefault 1 (String.toInt newId) }, Cmd.none )

        --JOS JE BOLJE DA SE RAZLOZI NA SLUCAJEVE I POSEBNO OBRADE UMJESTO Maybe.withDefault
        MsgSuccesfulPost result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, Cmd.none )

                --poruka = data, errorMessage = Nothing }, Cmd.none )
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgGotResults result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | results = data, errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgGotResult result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | resultBook = data, results = [], errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgSuccessfulDelete result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, Cmd.none )

                --poruka = data, errorMessage = Nothing }, Cmd.none )
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )


updateAddBook : Model -> ( Model, Cmd Msg )
updateAddBook model =
    ( { model | loading = True }, postBooks model )


updateDeleteBook : Model -> ( Model, Cmd Msg )
updateDeleteBook model =
    ( { model | loading = True }, cmdDeleteBook model )


updateShowBook : Model -> ( Model, Cmd Msg )
updateShowBook model =
    ( { model | loading = True }, cmdShowBook model )


updateSve : Model -> ( Model, Cmd Msg )
updateSve model =
    ( { model | loading = True }, cmdSearchAll )


updateElm : Model -> ( Model, Cmd Msg )
updateElm model =
    ( { model | loading = True }, cmdSearchElm )


updateHaskell : Model -> ( Model, Cmd Msg )
updateHaskell model =
    ( { model | loading = True }, cmdSearchHaskell )


updateElixir : Model -> ( Model, Cmd Msg )
updateElixir model =
    ( { model | loading = True }, cmdSearchElixir )


updateFsharp : Model -> ( Model, Cmd Msg )
updateFsharp model =
    ( { model | loading = True }, cmdSearchFsharp )


updateClojure : Model -> ( Model, Cmd Msg )
updateClojure model =
    ( { model | loading = True }, cmdSearchClojure )


updateOcaml : Model -> ( Model, Cmd Msg )
updateOcaml model =
    ( { model | loading = True }, cmdSearchOcaml )


updateScala : Model -> ( Model, Cmd Msg )
updateScala model =
    ( { model | loading = True }, cmdSearchScala )


updateRacket : Model -> ( Model, Cmd Msg )
updateRacket model =
    ( { model | loading = True }, cmdSearchRacket )


updateScheme : Model -> ( Model, Cmd Msg )
updateScheme model =
    ( { model | loading = True }, cmdSearchScheme )


updateLisp : Model -> ( Model, Cmd Msg )
updateLisp model =
    ( { model | loading = True }, cmdSearchLisp )


updateML : Model -> ( Model, Cmd Msg )
updateML model =
    ( { model | loading = True }, cmdSearchML )


updateAPL : Model -> ( Model, Cmd Msg )
updateAPL model =
    ( { model | loading = True }, cmdSearchAPL )


updateMiranda : Model -> ( Model, Cmd Msg )
updateMiranda model =
    ( { model | loading = True }, cmdSearchMiranda )


updateAgda : Model -> ( Model, Cmd Msg )
updateAgda model =
    ( { model | loading = True }, cmdSearchAgda )


updateErlang : Model -> ( Model, Cmd Msg )
updateErlang model =
    ( { model | loading = True }, cmdSearchErlang )


updateLargest : Model -> ( Model, Cmd Msg )
updateLargest model =
    ( { model | loading = True }, cmdSearchLargest )



--subscriptions : model -> Sub Msg
--subscriptions _ =
--    Browser.Events.onKeyPress keyPressed


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--keyPressed : JD.Decoder Msg
--keyPressed =
--    JD.map MsgKeyPressed (JD.field "key" JD.string)


viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { borderColor = Just (E.rgb255 0x00 0x33 0x66)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (E.column [ E.padding 2 ]
            [ viewSearchBar model
            , viewErrorMessage model
            , viewResults model
            , viewResult model
            ]
        )


viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.column []
        [ E.row [ E.spacing 10, E.paddingXY 10 30, EBG.color (E.rgb255 0x33 0x66 0x99) ]
            [ EI.search []
                { onChange = MsgInputTitleField
                , text = model.bookTitle
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Title:")
                }
            , EI.search []
                { onChange = MsgInputThumbnailField
                , text = model.bookThumbnail
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Thumbnail:")
                }
            , EI.search []
                { onChange = MsgInputPagesFieldAsString
                , text = String.fromInt model.bookPages --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Pages:")
                }
            , EI.search []
                { onChange = MsgInputLinkField
                , text = model.bookLink
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Link:")
                }
            , EI.search []
                { onChange = MsgInputPublisherField
                , text = model.bookPublisher
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Publisher:")
                }
            , EI.search []
                { onChange = MsgInputLanguageIdFieldAsString
                , text = String.fromInt model.bookLanguageId --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Language Id:")
                }
            , viewAddBookButton
            ]
        , E.row [ E.spacing 10, E.centerX, E.paddingXY 420 30, EBG.color (E.rgb255 0x33 0x66 0x99) ]
            [ EI.search []
                { onChange = MsgInputIdFieldAsString
                , text = String.fromInt model.bookId --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0xEE 0xEE 0xEE) ] (E.text "Book Id:")
                }
            , viewDeleteBookButton
            , viewShowBookButton
            ]
        , E.row [ E.spacing 2, E.paddingXY 0 5 ]
            [ viewGetBooksButton
            , viewGetBooksElmButton
            , viewGetBooksHaskellButton
            , viewGetBooksElixirButton
            , viewGetBooksFsharpButton
            , viewGetBooksClojureButton
            , viewGetBooksOcamlButton
            , viewGetBooksScalaButton
            , viewGetBooksRacketButton
            , viewGetBooksSchemeButton
            , viewGetBooksLispButton
            , viewGetBooksMLButton
            , viewGetBooksAPLButton
            , viewGetBooksMirandaButton
            , viewGetBooksAgdaButton
            , viewGetBooksErlangButton
            , viewGetBooksLargestButton
            , if model.loading then
                E.html loadingImage

              else
                E.none
            ]
        ]


loadingImage : Html.Html msg
loadingImage =
    S.svg
        [ SA.width "64px"
        , SA.height "64px"
        , SA.viewBox "0 0 48 48"
        ]
        [ S.circle
            [ SA.cx "24"
            , SA.cy "24"
            , SA.stroke "#6699AA"
            , SA.strokeWidth "4"
            , SA.r "8"
            , SA.fill "none"
            ]
            [ S.animate
                [ SA.attributeName "opacity"
                , SA.values "0;.8;0"
                , SA.dur "2s"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        ]


viewErrorMessage : Model -> E.Element msg
viewErrorMessage model =
    case model.errorMessage of
        Just errorMessage ->
            E.text errorMessage

        Nothing ->
            E.none


viewResults : Model -> E.Element msg
viewResults model =
    E.wrappedRow [ E.spacing 12, E.centerX ]
        (List.map viewBook model.results)


viewResult : Model -> E.Element msg
viewResult model =
    let
        bookPlaceholder =
            case model.resultBook of
                Just book ->
                    viewBook book

                Nothing ->
                    E.none
    in
    E.wrappedRow [ E.spacing 12, E.centerX ]
        [ bookPlaceholder ]


viewBook : Book -> E.Element msg
viewBook book =
    let
        titleE =
            E.paragraph [ EF.bold, EF.underline, E.paddingXY 0 12 ] [ E.text book.title ]

        thumbnailE =
            case book.thumbnail of
                Just thumbnail ->
                    viewBookCover thumbnail book.title

                Nothing ->
                    E.none

        pagesE =
            case book.pages of
                Just pages ->
                    E.paragraph [ EF.size 12 ]
                        [ E.text ("(" ++ String.fromInt pages ++ " pages)") ]

                Nothing ->
                    E.none

        publisherE =
            case book.publisher of
                Just publisher ->
                    E.paragraph [ EF.size 16 ]
                        [ E.text publisher ]

                Nothing ->
                    E.none
    in
    E.newTabLink
        [ E.width (E.px 360)
        , E.height (E.px 300)
        , EBG.color (E.rgb255 0xE3 0xEA 0xED)
        , EB.rounded 20
        , E.padding 10
        , E.mouseOver
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            ]
        ]
        { url = book.link
        , label =
            E.row [ E.centerX ]
                [ thumbnailE
                , E.column [ E.padding 20 ]
                    [ titleE
                    , publisherE
                    , pagesE
                    ]
                ]
        }


viewBookCover : String -> String -> E.Element msg
viewBookCover thumbnail title =
    E.image []
        { src = thumbnail
        , description = title
        }


viewButtonGeneric : String -> Msg -> E.Element Msg
viewButtonGeneric naziv msg =
    EI.button
        [ EBG.color (E.rgb255 0x00 0x33 0x66)
        , EF.color (E.rgb255 0xEE 0xEE 0xEE)
        , EB.rounded 5
        , E.padding 12
        , E.mouseOver
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        ]
        { onPress = Just msg
        , label = E.text naziv
        }


viewAddBookButton : E.Element Msg
viewAddBookButton =
    viewButtonGeneric "Add Book" MsgAddBook


viewDeleteBookButton : E.Element Msg
viewDeleteBookButton =
    viewButtonGeneric "Delete Book" MsgDeleteBook


viewShowBookButton : E.Element Msg
viewShowBookButton =
    viewButtonGeneric "Get book by Id" MsgShowBook


viewGetBooksButton : E.Element Msg
viewGetBooksButton =
    viewButtonGeneric "All" MsgGetBooks


viewGetBooksElmButton : E.Element Msg
viewGetBooksElmButton =
    viewButtonGeneric "Elm" MsgGetBooksElm


viewGetBooksHaskellButton : E.Element Msg
viewGetBooksHaskellButton =
    viewButtonGeneric "Haskell" MsgGetBooksHaskell


viewGetBooksElixirButton : E.Element Msg
viewGetBooksElixirButton =
    viewButtonGeneric "Elixir" MsgGetBooksElixir


viewGetBooksFsharpButton : E.Element Msg
viewGetBooksFsharpButton =
    viewButtonGeneric "F#" MsgGetBooksFsharp


viewGetBooksClojureButton : E.Element Msg
viewGetBooksClojureButton =
    viewButtonGeneric "Clojure" MsgGetBooksClojure


viewGetBooksOcamlButton : E.Element Msg
viewGetBooksOcamlButton =
    viewButtonGeneric "OCaml" MsgGetBooksOcaml


viewGetBooksScalaButton : E.Element Msg
viewGetBooksScalaButton =
    viewButtonGeneric "Scala" MsgGetBooksScala


viewGetBooksRacketButton : E.Element Msg
viewGetBooksRacketButton =
    viewButtonGeneric "Racket" MsgGetBooksRacket


viewGetBooksSchemeButton : E.Element Msg
viewGetBooksSchemeButton =
    viewButtonGeneric "Scheme" MsgGetBooksScheme


viewGetBooksLispButton : E.Element Msg
viewGetBooksLispButton =
    viewButtonGeneric "Lisp" MsgGetBooksLisp


viewGetBooksMLButton : E.Element Msg
viewGetBooksMLButton =
    viewButtonGeneric "ML" MsgGetBooksML


viewGetBooksAPLButton : E.Element Msg
viewGetBooksAPLButton =
    viewButtonGeneric "APL" MsgGetBooksAPL


viewGetBooksMirandaButton : E.Element Msg
viewGetBooksMirandaButton =
    viewButtonGeneric "Miranda" MsgGetBooksMiranda


viewGetBooksAgdaButton : E.Element Msg
viewGetBooksAgdaButton =
    viewButtonGeneric "Agda" MsgGetBooksAgda


viewGetBooksErlangButton : E.Element Msg
viewGetBooksErlangButton =
    viewButtonGeneric "Erlang" MsgGetBooksErlang


viewGetBooksLargestButton : E.Element Msg
viewGetBooksLargestButton =
    viewButtonGeneric "10 Largest" MsgGetBooksLargest



--NE KORISTIM OVAJ, ZBOG HEDERA NEODGOVARAJUCIH ZA MOJ SERVER


cmdAddBook : Model -> Cmd Msg
cmdAddBook model =
    Http.request
        { method = "POST"
        , headers = [ header "Access-Control-Allow-Origin" "http://localhost:5000", header "Access-Control-Allow-Credentials" "true", header "Content-Type" "application/json" ]
        , url = "http://localhost:5000/books"
        , body = jsonBody (encode model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        , timeout = Nothing
        , tracker = Nothing
        }


postBooks : Model -> Cmd Msg
postBooks model =
    Http.post
        { url = "http://localhost:5000/books"
        , body = jsonBody (encode model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        }


cmdDeleteBook : Model -> Cmd Msg
cmdDeleteBook model =
    Http.post
        { url = "http://localhost:5000/books/" ++ String.fromInt model.bookId
        , body = emptyBody
        , expect = Http.expectWhatever MsgSuccessfulDelete
        }


cmdShowBook : Model -> Cmd Msg
cmdShowBook model =
    Http.get
        { url = "http://localhost:5000/books/" ++ String.fromInt model.bookId
        , expect = Http.expectJson MsgGotResult (JD.maybe decodeItem)
        }


cmdSearchAll : Cmd Msg
cmdSearchAll =
    Http.get
        { url = "http://localhost:5000/books/sve"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchElm : Cmd Msg
cmdSearchElm =
    Http.get
        { url = "http://localhost:5000/books/elm"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchHaskell : Cmd Msg
cmdSearchHaskell =
    Http.get
        { url = "http://localhost:5000/books/haskell"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchElixir : Cmd Msg
cmdSearchElixir =
    Http.get
        { url = "http://localhost:5000/books/elixir"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchFsharp : Cmd Msg
cmdSearchFsharp =
    Http.get
        { url = "http://localhost:5000/books/fsharp"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchClojure : Cmd Msg
cmdSearchClojure =
    Http.get
        { url = "http://localhost:5000/books/clojure"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchOcaml : Cmd Msg
cmdSearchOcaml =
    Http.get
        { url = "http://localhost:5000/books/ocaml"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchScala : Cmd Msg
cmdSearchScala =
    Http.get
        { url = "http://localhost:5000/books/scala"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchRacket : Cmd Msg
cmdSearchRacket =
    Http.get
        { url = "http://localhost:5000/books/racket"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchScheme : Cmd Msg
cmdSearchScheme =
    Http.get
        { url = "http://localhost:5000/books/scheme"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchLisp : Cmd Msg
cmdSearchLisp =
    Http.get
        { url = "http://localhost:5000/books/lisp"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchML : Cmd Msg
cmdSearchML =
    Http.get
        { url = "http://localhost:5000/books/ml"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchAPL : Cmd Msg
cmdSearchAPL =
    Http.get
        { url = "http://localhost:5000/books/apl"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchMiranda : Cmd Msg
cmdSearchMiranda =
    Http.get
        { url = "http://localhost:5000/books/miranda"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchAgda : Cmd Msg
cmdSearchAgda =
    Http.get
        { url = "http://localhost:5000/books/agda"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchErlang : Cmd Msg
cmdSearchErlang =
    Http.get
        { url = "http://localhost:5000/books/erlang"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchLargest : Cmd Msg
cmdSearchLargest =
    Http.get
        { url = "http://localhost:5000/books/join"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "title", JE.string model.bookTitle )
        , ( "thumbnail", JE.string model.bookThumbnail )
        , ( "pages", JE.int model.bookPages )
        , ( "link", JE.string model.bookLink )
        , ( "publisher", JE.string model.bookPublisher )
        , ( "languageId", JE.int model.bookLanguageId )
        ]


decodeItems : JD.Decoder (List Book)
decodeItems =
    JD.list decodeItem


decodeItem : JD.Decoder Book
decodeItem =
    JD.map5 Book
        (JD.field "title" JD.string)
        (JD.maybe (JD.field "thumbnail" JD.string))
        (JD.field "link" JD.string)
        (JD.maybe (JD.field "pages" JD.int))
        (JD.maybe (JD.field "publisher" JD.string))
