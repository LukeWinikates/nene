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
-- TODO: don't reload the whole grid when saving attestation
-- TODO: provide icons for moving back and forth/resizing the different panels
-- TODO: card UI:
-- attestation: dictionary-word internet-examples unattested hard-to-pronounce unlikely impossible
-- tags/feelings (freeform words? allow for hashtags?)


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
    attestWord word
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
    VowelWiseGrouping (ConsonantWiseGrouping Word)


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
    = Save String
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
                    ( { model | page = Open }
                        |> clearNotification
                    , (saveWord word)
                    )

                SaveComplete (Err error) ->
                    { model | notificationText = Just (toString error) }
                        |> withCommand loadGojuonGrid

                SaveComplete (Ok _) ->
                    model
                        |> withCommand loadGojuonGrid
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


wordView : Word -> Html Msg
wordView word =
    div [ class "card" ]
        [ strong [ style [ ( "font-size", "16px" ) ] ] [ text word.kana ]
        , em [] [ text word.romaji ]
        , button [ onClick (CloseWord word), class "hovers", style [ ( "float", "right" ) ] ] [ text "x" ]
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


detailedItemView : Page -> Word -> Html Msg
detailedItemView currentPage word =
    div
        [ classList [ ( "word-square", True ), ( "attested", word.attested ), ( "hovers", True ) ]
        , onClick (PageChange <| (addCard currentPage word))
        ]
        [ text word.kana ]


itemView : Word -> Html Msg
itemView word =
    div
        [ classList [ ( "word-square", True ), ( "attested", word.attested ) ] ]
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



-- TODO: these should probably indicate what their parent is, e.g. they're all: あ＿あ＿ or げ＿げ＿


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
                            , onClick (PageChange <| WithSection vg)
                            ]
                            (text vg.dan :: (List.map secondMoraGroupings vg.items))
                    )
                    consonantGroup.items
               )
        )


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
    section [ style [ ( "display", "flex" ), ( "width", "120vw" ), ( "transition", ".3s left" ), ( "position", "relative" ), ( "left", leftOffset ) ] ]
        [ (section [ style [ ( "width", "40vw" ), ( "display", "inline-block" ) ], classList [ ( "portal", True ) ] ] <|
            left
          )
        , section [ style [ ( "width", "60vw" ), ( "display", "inline-block" ) ] ]
            center
        , section [ style [ ( "width", "20vw" ), ( "display", "inline-block" ) ] ]
            right
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
                    layout "-20vw" (gojuonView gojuon) [ (activeRowView model.page selection) ] [ empty ]

                WithSectionAndCards selection cards ->
                    layout "-30vw" (gojuonView gojuon) [ (activeRowView model.page selection) ] [ cardsView cards ]

        Nothing ->
            text "waiting for data to load..."


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , pageView (model)
        ]
