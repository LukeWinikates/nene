module Main exposing (..)

import Html exposing (Html, article, button, div, em, header, input, section, span, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Dict
import Maybe exposing (withDefault)
import Ajax exposing (..)


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


type ViewportPosition
    = Left
    | Center
    | Right


type alias PageState =
    { viewportPosition : ViewportPosition
    , selection : Maybe Selection
    , words : List Word
    }


defaultPageState =
    { viewportPosition = Left
    , selection = Nothing
    , words = []
    }


loadGojuonGrid =
    Http.send LoadGojuonGrid (Http.get "/api/words" gojuonDecoder)


saveWord word attestationType =
    attestWord word.kana attestationType
        |> Http.send
            (SaveComplete >> Attesting)


type Page
    = Explorer PageState


addCard : Page -> Word -> Page
addCard page word =
    case page of
        Explorer pageState ->
            Explorer { pageState | words = pageState.words ++ [ word ], viewportPosition = Right }


type alias Selection =
    { gyo : String
    , dan : String
    }


type alias Model =
    { notificationText : Maybe String
    , gojuon : Maybe GitaigoByGojuonOrder
    , page : Page
    }


emptyModel : Model
emptyModel =
    { notificationText = Nothing
    , gojuon = Nothing
    , page = Explorer defaultPageState
    }


type AttestingEvent
    = Save Word Attestation
    | SaveComplete (Result Http.Error Bool)


type Msg
    = LoadGojuonGrid (Result Http.Error GitaigoByGojuonOrder)
    | PageChange Page
    | Attesting AttestingEvent
    | CloseWord Word
    | ChangeViewport ViewportPosition


closeWord : Word -> Page -> Page
closeWord word page =
    case page of
        Explorer pageState ->
            Explorer { pageState | words = (List.filter ((/=) word) pageState.words) }


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


replaceCard : Page -> Word -> Page
replaceCard page word =
    case page of
        Explorer pageState ->
            Explorer { pageState | words = (replaceIf (.kana >> ((==) word.kana)) (always word) pageState.words) }


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
                            | page = replaceCard model.page newWord
                            , gojuon =
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

        CloseWord word ->
            { model | page = closeWord word model.page }
                |> noCommand

        ChangeViewport position ->
            { model
                | page =
                    case model.page of
                        Explorer pageState ->
                            Explorer { pageState | viewportPosition = position }
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
        , button [ onClick (CloseWord word), class "hovers pull-right" ] [ text "x" ]
        , attestationIndicator word
        ]


cssClassFromWord : Word -> ( String, Bool )
cssClassFromWord word =
    ( attestationToString word.attestation, True )


itemViewWithText : Page -> Word -> Html Msg
itemViewWithText currentPage word =
    div
        [ classList
            [ ( "word-square", True )
            , (cssClassFromWord word)
            , ( "hovers", True )
            ]
        , onClick (PageChange <| (addCard currentPage word))
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


detailedSecondMoraGroupings : Page -> ConsonantWiseGrouping Word -> Html Msg
detailedSecondMoraGroupings page consonantGroup =
    innerConsonantGroupingView
        "select-grouping-word-row"
        (itemViewWithText page)
        consonantGroup


listWithTitle : String -> (a -> Html Msg) -> List a -> List (Html Msg)
listWithTitle title itemView items =
    div [] [(text title)] :: (List.map itemView items)


thumbnailVowelDanColumnView : String -> VowelWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
thumbnailVowelDanColumnView gyo vg =
    div
        [ class "hovers thumbnail-dan-column"
        , onClick
            (PageChange <|
                Explorer
                    { defaultPageState
                        | selection = Just { gyo = gyo, dan = vg.dan }
                        , viewportPosition = Center
                    }
            )
        ]
    <|
        listWithTitle vg.dan secondMoraGroupings vg.items


thumbnailFirstConsonantGyoView : ConsonantWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
thumbnailFirstConsonantGyoView consonantGroup =
    section [ class "card" ] <|
        listWithTitle
            consonantGroup.gyo
            (thumbnailVowelDanColumnView consonantGroup.gyo)
            consonantGroup.items


getVowelWiseGrouping : GitaigoByGojuonOrder -> Selection -> Maybe (VowelWiseGrouping (ConsonantWiseGrouping Word))
getVowelWiseGrouping gojuon selection =
    gojuon
        |> List.filter (gyoEq selection.gyo)
        |> List.head
        |> Maybe.map .items
        |> Maybe.withDefault []
        |> List.filter (danEq selection.dan)
        |> List.head


activeRowView : Page -> VowelWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
activeRowView page grouping =
    section [ class "card" ] <|
        listWithTitle grouping.dan (detailedSecondMoraGroupings page) grouping.items


gojuonThumbnailView gojuon =
    List.map thumbnailFirstConsonantGyoView gojuon


cardsView : List Word -> Html Msg
cardsView words =
    div [] <|
        List.map wordView words


classes : List String -> Html.Attribute msg
classes =
    (String.join " ") >> class


viewportPositionToString : ViewportPosition -> String
viewportPositionToString vp =
    case vp of
        Left ->
            "left"

        Center ->
            "center"

        Right ->
            "right"


layoutElement : ViewportPosition -> String -> List (Html Msg) -> Html Msg
layoutElement viewportPosition title elements =
    let
        viewportString =
            (viewportPositionToString viewportPosition)
    in
        (section [ classes [ "layout-element", "layout-" ++ viewportString ] ] <|
            (header
                [ classes [ viewportString ++ "-header", "hovers" ]
                , onClick (ChangeViewport viewportPosition)
                ]
                [ text title ]
            )
                :: elements
        )


layout : PageState -> List (Html Msg) -> List (Html Msg) -> List (Html Msg) -> Html Msg
layout pageState left center right =
    section
        [ classes [ "layout", (leftViewportClassForPageState pageState) ] ]
        [ layoutElement Left "All" left
        , layoutElement Center "Selected" center
        , layoutElement Right "Words" right
        ]


empty =
    text ""


leftViewportClassForPageState : PageState -> String
leftViewportClassForPageState pageState =
    case pageState.viewportPosition of
        Left ->
            "layout-viewport-left"

        Center ->
            "layout-viewport-center"

        Right ->
            "layout-viewport-right"


pageView : Model -> Html Msg
pageView model =
    case model.gojuon of
        Just gojuon ->
            case model.page of
                Explorer pageState ->
                    layout
                        pageState
                        (gojuonThumbnailView gojuon)
                        [ pageState.selection
                            |> Maybe.andThen (getVowelWiseGrouping gojuon)
                            |> Maybe.map (activeRowView model.page)
                            |> Maybe.withDefault empty
                        ]
                        [ cardsView pageState.words ]

        Nothing ->
            text "waiting for data to load..."


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , pageView (model)
        ]
