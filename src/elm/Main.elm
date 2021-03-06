module Main exposing (..)

import Html exposing (Attribute, Html, article, button, div, em, header, input, li, option, section, select, span, strong, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Dict
import Maybe exposing (withDefault)
import Ajax exposing (..)


-- TODO: don't allow background to scroll when modal open
-- TODO: fix the performance issue with the searchbox
-- TODO: support rendaku (さめざめ)
-- TODO: support っ between the mora, like あっぷあっぷ
-- TODO: change view to show more detailed word cards (instead of just the kana)
-- TODO: instead of gyo + dan headers, show あ＿　or き＿ as the labels
-- TODO: these should probably indicate what their parent is, e.g. they're all: あ＿あ＿ or げ＿げ＿
-- TODO: refactor the clojure code for combining consonants with vowels
-- TODO: theme: make public
-- TODO: get setup to run on heroku, write a lein task that pushes attestations from the local DB to the cloud instance
-- TODO: make the app only allow me to edit things when it's on heroku.
-- TODO: theme: associations
-- TODO: groupings by other relationships, alternative ways of visualizing the sounds spaces
-- TODO: start to classify associations between phonemes and types of sounds/feelings
-- TODO: tags/feelings (freeform words? allow for hashtags?)
-- TODO: theme: non-doubling
-- TODO: think about how to deal with non-doubling ones, like "しゃんと", for example
-- TODO: just show one indicator for the lone mora, ahead of the lists of combined ones
-- TODO: why are some doubling-words used with と and some not?
-- TODO: theme: usability/features
-- TODO: indicate subtype of dictionary word - gitaigo, giseigo, non-gitago doubling words. can be multiple types at once.
-- TODO: add a search box
-- TODO: introduce routing so refreshes work
-- TODO: think about switching everything to katakana (or knowing which one is right for the thing in question...)
-- TODO: distinctions between different kinds of words (gitaigo, giseigo, giongo, gijougo)
-- TODO: maybe the "Attestation" elm type vs "attestation.type" naming in the sql table can be harmonized
-- TODO: since internet examples is really only "ginigini", that's probably not a thing... or it's slightly less than a dictionary word. Maybe just attach markdown to each thing?
-- TODO: markdown renderer for notes on words


main =
    Html.program
        { view = view
        , update = update
        , init = ( emptyModel, loadGojuonGrid )
        , subscriptions = always Sub.none
        }


type alias PageState =
    { selection : Maybe Selection
    , word : Maybe Word
    }


defaultPageState =
    { selection = Nothing
    , word = Nothing
    }


loadGojuonGrid =
    Http.send LoadGojuonGrid (Http.get "/api/words" gojuonDecoder)


saveWord word attestationType =
    attestWord word.kana attestationType
        |> Http.send
            (SaveComplete >> Attesting)


type Page
    = Explorer PageState


type alias Selection =
    { gyo : String
    , dan : String
    }


type alias Model =
    { notificationText : Maybe String
    , gojuon : Maybe GitaigoByGojuonOrder
    , page : Page
    , search : Maybe String
    }


emptyModel : Model
emptyModel =
    { notificationText = Nothing
    , gojuon = Nothing
    , page = Explorer defaultPageState
    , search = Nothing
    }


type AttestingEvent
    = Save Word Attestation
    | SaveComplete (Result Http.Error Bool)


type Msg
    = LoadGojuonGrid (Result Http.Error GitaigoByGojuonOrder)
    | PageChange Page
    | Attesting AttestingEvent
    | CloseWord Word
    | OpenWord Word
    | CloseModal
    | SearchTerm String


closeWord : Page -> Page
closeWord page =
    case page of
        Explorer pageState ->
            Explorer { pageState | word = Nothing }


danEq x =
    .dan >> ((==) x)


gyoEq x =
    .gyo >> ((==) x)


swapItems f x =
    { x | items = f (x.items) }


replaceIf : (a -> Bool) -> (a -> a) -> List a -> List a
replaceIf pred f list =
    List.map
        (\a ->
            if pred a then
                (f a)
            else
                a
        )
        list


words : GitaigoByGojuonOrder -> List Word
words gojuon =
    gojuon
        |> List.concatMap .items
        |> List.concatMap .items
        |> List.concatMap .items
        |> List.concatMap .items


wordContains : String -> Word -> Bool
wordContains text word =
    String.contains text word.kana || String.contains (String.toLower text) word.romaji


findWords : GitaigoByGojuonOrder -> String -> List Word
findWords gojuon searchTerm =
    words gojuon
        |> List.filter (wordContains searchTerm)
        |> List.take 5


attest : Model -> Word -> Attestation -> Model
attest model word attestationType =
    let
        newWord =
            { word | attestation = attestationType }
    in
        case word.location of
            [ g1, d1, g2, d2 ] ->
                case model.gojuon of
                    Just gojuon ->
                        { model
                            | gojuon =
                                Just
                                    (replaceIf
                                        (gyoEq g1)
                                        (swapItems
                                            (replaceIf
                                                (danEq d1)
                                                (swapItems
                                                    (replaceIf
                                                        (gyoEq g2)
                                                        (swapItems
                                                            (replaceIf
                                                                (danEq d2)
                                                                (swapItems
                                                                    (always [ newWord ])
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                        gojuon
                                    )
                        }

                    _ ->
                        model

            _ ->
                model


update msg model =
    (case msg of
        LoadGojuonGrid (Err er) ->
            { model | notificationText = Just <| toString er }
                |> noCommand

        LoadGojuonGrid (Ok categories) ->
            { model | notificationText = Nothing, gojuon = Just categories }
                |> noCommand

        PageChange page ->
            { model | page = page }
                |> clearNotification
                |> noCommand

        OpenWord word ->
            { model | page = setWord word model.page }
                |> clearNotification
                |> noCommand

        CloseWord word ->
            { model | page = closeWord model.page }
                |> noCommand

        CloseModal ->
            { model | page = Explorer defaultPageState }
                |> noCommand

        SearchTerm term ->
            { model
                | search =
                    if term /= "" then
                        Just term
                    else
                        Nothing
            }
                |> noCommand

        Attesting e ->
            case e of
                Save word attestationType ->
                    ( (attest model word attestationType)
                        |> clearNotification
                    , (saveWord word attestationType)
                    )

                SaveComplete (Err error) ->
                    { model | notificationText = Just (toString error) }
                        |> withCommand loadGojuonGrid

                SaveComplete (Ok _) ->
                    model
                        |> noCommand
    )


clearNotification : Model -> Model
clearNotification model =
    { model | notificationText = Nothing }


noCommand : Model -> ( Model, Cmd Msg )
noCommand model =
    ( model, Cmd.none )


withCommand : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCommand =
    flip (,)


indicator : Word -> Attestation -> Html Msg
indicator word thisAttestation =
    button
        [ classList
            [ ( "hovers", True )
            , ( (attestationToString thisAttestation), word.attestation == thisAttestation )
            ]
        , onClick (Attesting (Save word thisAttestation))
        ]
        [ text <| attestationToString thisAttestation ]


attestationIndicator : Word -> Html Msg
attestationIndicator word =
    div
        [ class "attestation-indicator" ]
    <|
        List.map
            (indicator word)
            [ DictionaryWord, InternetExamples, Unattested, HardToPronounce, Unlikely, Impossible ]


wordView : Word -> Html Msg
wordView word =
    div [ class "card word-card" ]
        [ strong [] [ text word.kana ]
        , em [] [ text word.romaji ]
        , attestationIndicator word
        ]


cssClassFromWord : Word -> ( String, Bool )
cssClassFromWord word =
    ( attestationToString word.attestation, True )


setWord : Word -> Page -> Page
setWord word page =
    case page of
        Explorer pageState ->
            Explorer { pageState | word = Just word }


itemViewWithText : Word -> Html Msg
itemViewWithText word =
    div
        [ classList
            [ ( "word-square", True )
            , (cssClassFromWord word)
            , ( "hovers", True )
            ]
        , onClick (OpenWord word)
        ]
        [ text word.kana ]


itemSquareView : Word -> Html Msg
itemSquareView word =
    div
        [ classList [ ( "word-square", True ), (cssClassFromWord word) ] ]
        []


innerConsonantGroupingView : String -> (Word -> Html Msg) -> ConsonantWiseGrouping Word -> Html Msg
innerConsonantGroupingView rowClass wordView consonantGroup =
    section [] <|
        List.map
            (\vg -> div [ class rowClass ] (List.map wordView vg.items))
            consonantGroup.items


secondMoraGroupings : ConsonantWiseGrouping Word -> Html Msg
secondMoraGroupings consonantGroup =
    innerConsonantGroupingView
        "gojuon-thumbnail-word-row"
        itemSquareView
        consonantGroup


detailedSecondMoraGroupings : ConsonantWiseGrouping Word -> Html Msg
detailedSecondMoraGroupings consonantGroup =
    innerConsonantGroupingView
        "select-grouping-word-row"
        itemViewWithText
        consonantGroup


listWithTitle : String -> (a -> Html Msg) -> List a -> List (Html Msg)
listWithTitle title itemView items =
    div [] [ (text title) ] :: (List.map itemView items)


thumbnailVowelDanColumnView : String -> VowelWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
thumbnailVowelDanColumnView gyo vg =
    div
        [ class "hovers thumbnail-dan-column"
        , onClick
            (PageChange <|
                Explorer
                    { defaultPageState
                        | selection = Just { gyo = gyo, dan = vg.dan }
                    }
            )
        ]
    <|
        listWithTitle vg.dan secondMoraGroupings vg.items


thumbnailFirstConsonantGyoView : ConsonantWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
thumbnailFirstConsonantGyoView consonantGroup =
    section [ classList [ ( "card", True ) ] ] <|
        listWithTitle
            consonantGroup.gyo
            (thumbnailVowelDanColumnView consonantGroup.gyo)
            consonantGroup.items


selectionToVowelWiseGrouping : GitaigoByGojuonOrder -> Selection -> Maybe (VowelWiseGrouping (ConsonantWiseGrouping Word))
selectionToVowelWiseGrouping gojuon selection =
    gojuon
        |> List.filter (gyoEq selection.gyo)
        |> List.head
        |> Maybe.map .items
        |> Maybe.withDefault []
        |> List.filter (danEq selection.dan)
        |> List.head


defaultWordFromVowelWiseGrouping : VowelWiseGrouping (ConsonantWiseGrouping Word) -> Maybe Word
defaultWordFromVowelWiseGrouping vowelWiseGrouping =
    vowelWiseGrouping
        |> .items
        |> List.head
        |> Maybe.map .items
        |> Maybe.andThen List.head
        |> Maybe.map .items
        |> Maybe.andThen List.head


(|:|) : Maybe a -> Maybe a -> Maybe a
(|:|) firstOption secondOption =
    case firstOption of
        Just _ ->
            firstOption

        Nothing ->
            secondOption


activeRowView : Maybe Word -> VowelWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
activeRowView maybeWord grouping =
    article []
        [ header []
            [ text grouping.dan
            , (button [ onClick CloseModal, class "pull-right icon-button" ]) [ text "x" ]
            ]
        , section []
            [ maybeWord
                |:| defaultWordFromVowelWiseGrouping grouping
                |> Maybe.map wordView
                |> Maybe.withDefault empty
            ]
        , section [] <| List.map detailedSecondMoraGroupings grouping.items
        ]


gojuonThumbnailView gojuon =
    section [] <|
        List.map (thumbnailFirstConsonantGyoView) gojuon


cardsView : List Word -> Html Msg
cardsView words =
    div [] <|
        List.map wordView words


classes : List String -> Html.Attribute msg
classes =
    (String.join " ") >> class


empty =
    text ""


searchBox : Maybe GitaigoByGojuonOrder -> Maybe String -> Html Msg
searchBox maybeGojuon searchTerm =
    div [ class "search-box" ]
        [ input [ value (searchTerm |> Maybe.withDefault ""), onInput SearchTerm ] []
        , section [ class "search-results" ] <|
            List.map wordView
                (Maybe.map2 findWords maybeGojuon searchTerm
                    |> Maybe.withDefault []
                )
        ]


modalView : Maybe Word -> VowelWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
modalView maybeWord selection =
    div
        [ class "modal-container" ]
        [ div [ class "modal-background", onClick CloseModal ] []
        , div
            [ class "modal" ]
            [ activeRowView maybeWord selection ]
        ]


pageView : Model -> Html Msg
pageView model =
    case model.gojuon of
        Just gojuon ->
            case model.page of
                Explorer pageState ->
                    div []
                        [ gojuonThumbnailView gojuon
                        , pageState.selection
                            |> Maybe.andThen (selectionToVowelWiseGrouping gojuon)
                            |> Maybe.map (modalView pageState.word)
                            |> withDefault empty
                        ]

        Nothing ->
            text "waiting for data to load..."


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , searchBox model.gojuon model.search
        , pageView (model)
        ]
