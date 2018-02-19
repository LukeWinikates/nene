module Main exposing (..)

import Html exposing (Html, article, button, div, em, header, input, section, span, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Dict
import Maybe exposing (withDefault)
import Ajax exposing (..)


-- TODO: separate data structure for tracking the viewport over the 160vw space?
-- TODO: negative attestations, with types (unpronounceable, awkward)
-- TODO: provide icons for moving back and forth/resizing the different panels
-- TODO: card UI:
-- attestation: dictionary-word internet-examples unattested hard-to-pronounce unlikely impossible
-- tags/feelings (freeform words? allow for hashtags?)
-- TODO: these should probably indicate what their parent is, e.g. they're all: あ＿あ＿ or げ＿げ＿
-- TODO: refactor out the duplication in the various gojuon subviews
-- TODO: introduce routing so refreshes work
-- TODO: do something for the viewport navigation/resizing
-- TODO: attestation should use kana


main =
    Html.program
        { view = view
        , update = update
        , init = ( emptyModel, loadGojuonGrid )
        , subscriptions = always Sub.none
        }


loadGojuonGrid =
    Http.send LoadGojuonGrid (Http.get "/api/words" gojuonDecoder)


saveWord word =
    attestWord word.romaji
        |> Http.send
            (SaveComplete >> Attesting)


type Page
    = Open
    | WithSection Selection
    | WithSectionAndCards Selection (List Word)


addCard : Page -> Word -> Page
addCard page word =
    case page of
        Open ->
            Open

        WithSection section ->
            WithSectionAndCards section [ word ]

        WithSectionAndCards section currentWords ->
            WithSectionAndCards section (currentWords ++ [ word ])


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
    , page = Open
    }


type AttestingEvent
    = Save Word
    | SaveComplete (Result Http.Error Bool)


type Msg
    = LoadGojuonGrid (Result Http.Error GitaigoByGojuonOrder)
    | PageChange Page
    | Attesting AttestingEvent
    | CloseWord Word


closeWord : Word -> Page -> Page
closeWord word page =
    case page of
        WithSectionAndCards section cards ->
            WithSectionAndCards section (List.filter ((/=) word) cards)

        _ ->
            page


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
        WithSectionAndCards s c ->
            WithSectionAndCards s (replaceIf (.kana >> ((==) word.kana)) (always word) c)

        _ ->
            page


attest : Model -> Word -> Model
attest model word =
    let
        newWord =
            { word | attestation = DictionaryWord }
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

        Attesting e ->
            case e of
                Save word ->
                    ( (attest model word)
                        |> clearNotification
                    , (saveWord word)
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


attestationIndicator : Word -> Html Msg
attestationIndicator word =
    case word.attestation of
        DictionaryWord ->
            div [] []

        Unattested ->
            button [ class "hovers", onClick (Attesting (Save word)) ] [ text "attest" ]

        _ ->
            empty


wordView : Word -> Html Msg
wordView word =
    div [ class "card" ]
        [ strong [ style [ ( "font-size", "16px" ) ] ] [ text word.kana ]
        , em [] [ text word.romaji ]
        , button [ onClick (CloseWord word), class "hovers", style [ ( "float", "right" ) ] ] [ text "x" ]
        , attestationIndicator word
        ]


groupView : Group -> Html Msg
groupView category =
    article [ class "card" ]
        [ section [ class "card-label vertical" ]
            [ div [] [ text category.jp ]
            , div [] [ text category.en ]
            ]
        , section [ class "card-content pill-container" ]
            (List.map wordView category.words)
        ]


secondLevelConsonantView : ConsonantWiseGrouping Word -> Html Msg
secondLevelConsonantView consonantGroup =
    section [ style [ ( "width", "100%" ) ] ] <|
        (div [ style [] ] [ (text consonantGroup.gyo) ])
            :: List.map
                (\vg ->
                    div [ style [ ( "height", "20px" ), ( "width", "20px" ) ] ] []
                )
                consonantGroup.items


cssClassFromWord : Word -> ( String, Bool )
cssClassFromWord word =
    (flip (,)) True <|
        case word.attestation of
            DictionaryWord ->
                "attested-dictionary-word"

            _ ->
                ""


detailedItemView : Page -> Word -> Html Msg
detailedItemView currentPage word =
    div
        [ classList [ ( "word-square", True ), (cssClassFromWord word), ( "hovers", True ) ]
        , onClick (PageChange <| (addCard currentPage word))
        ]
        [ text word.kana ]


itemView : Word -> Html Msg
itemView word =
    div
        [ classList [ ( "word-square", True ), (cssClassFromWord word) ] ]
        []


secondMoraGroupings : ConsonantWiseGrouping Word -> Html Msg
secondMoraGroupings consonantGroup =
    section []
        (List.map
            (\vg ->
                div
                    [ style
                        [ ( "height", "5px" )
                        , ( "width", "12%" )
                        , ( "display", "inline-block" )
                        , ( "font-size", "8px" )
                        ]
                    ]
                    (List.map itemView vg.items)
            )
            consonantGroup.items
        )


detailedSecondMoraGroupings : Page -> ConsonantWiseGrouping Word -> Html Msg
detailedSecondMoraGroupings page consonantGroup =
    section [ style [ ( "width", "100%" ) ] ]
        (List.map
            (\vg ->
                div
                    [ style
                        [ ( "height", "25px" )
                        , ( "width", "12%" )
                        , ( "display", "inline-block" )
                        , ( "font-size", "12px" )
                        ]
                    ]
                    (List.map (detailedItemView page) vg.items)
            )
            consonantGroup.items
        )


firstLevelConsonantView : ConsonantWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
firstLevelConsonantView consonantGroup =
    section [ class "card" ]
        (div
            []
            [ (text consonantGroup.gyo) ]
            :: (List.map
                    (\vg ->
                        div
                            [ style
                                [ ( "display", "inline-block" )
                                , ( "width", "12.5%" )
                                ]
                            , class "hovers"
                            , onClick (PageChange <| WithSection { gyo = consonantGroup.gyo, dan = vg.dan })
                            ]
                            (text vg.dan :: (List.map secondMoraGroupings vg.items))
                    )
                    consonantGroup.items
               )
        )


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
    section
        [ class "card" ]
        ((div []
            [ text
                grouping.dan
            ]
         )
            :: (List.map (detailedSecondMoraGroupings page) grouping.items)
        )


gojuonView gojuon =
    List.map firstLevelConsonantView gojuon


cardsView : List Word -> Html Msg
cardsView words =
    div [] <|
        List.map wordView words


layout : String -> List (Html Msg) -> List (Html Msg) -> List (Html Msg) -> Html Msg
layout leftOffset left center right =
    section
        [ style
            [ ( "display", "flex" )
            , ( "width", "120vw" )
            , ( "transition", ".3s left" )
            , ( "position", "relative" )
            , ( "left", leftOffset )
            ]
        , class "layout"
        ]
        [ (section [ style [ ( "width", "40vw" ), ( "display", "inline-block" ) ], classList [ ( "portal", True ) ] ] <|
            (header [ class "left-header" ] [ text "All" ])
                :: left
          )
        , section [ style [ ( "width", "60vw" ), ( "display", "inline-block" ) ] ]
            ((header [ class "center-header" ] [ text "Selected" ])
                :: center
            )
        , section [ style [ ( "width", "20vw" ), ( "display", "inline-block" ) ] ]
            ((header [ class "right-header" ] [ text "Words" ])
                :: right
            )
        ]


empty =
    text ""


pageView : Model -> Html Msg
pageView model =
    case model.gojuon of
        Just gojuon ->
            case model.page of
                Open ->
                    layout "0" (gojuonView gojuon) [ empty ] [ empty ]

                WithSection selection ->
                    layout "-20vw"
                        (gojuonView gojuon)
                        [ (Maybe.map (activeRowView model.page) (getVowelWiseGrouping gojuon selection)) |> Maybe.withDefault empty ]
                        [ empty ]

                WithSectionAndCards selection cards ->
                    layout "-30vw"
                        (gojuonView gojuon)
                        [ (Maybe.map (activeRowView model.page) (getVowelWiseGrouping gojuon selection)) |> Maybe.withDefault empty ]
                        [ cardsView cards ]

        Nothing ->
            text "waiting for data to load..."


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , pageView (model)
        ]
