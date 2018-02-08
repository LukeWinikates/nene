module Main exposing (..)

import Html exposing (Html, article, button, div, em, header, input, section, span, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Dict
import Maybe exposing (withDefault)
import Ajax exposing (..)


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
    Http.send
        (SaveComplete >> Attesting)
    <|
        attestWord word


type Page
    = Explorer (Maybe (VowelWiseGrouping (ConsonantWiseGrouping Word)))


type alias Model =
    { notificationText : Maybe String
    , gojuon : Maybe GitaigoByGojuonOrder
    , page : Page
    }


emptyModel : Model
emptyModel =
    { notificationText = Nothing
    , gojuon = Nothing
    , page = Explorer Nothing
    }



-- TODO: negative attestations, with types (unpronounceable, awkward)


type AttestingEvent
    = Save String
    | SaveComplete (Result Http.Error Bool)


type Msg
    = LoadGojuonGrid (Result Http.Error GitaigoByGojuonOrder)
    | PageChange Page
    | Attesting AttestingEvent


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

        Attesting e ->
            case e of
                Save word ->
                    ( { model | page = Explorer Nothing }
                        |> clearNotification
                    , (saveWord word)
                    )

                -- TODO: don't reload the whole grid here
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
    div [ class "pill" ]
        [ strong [ style [ ( "font-size", "16px" ) ] ] [ text word.kana ]
        , em [] [ text word.romaji ]
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


detailedItemView : Word -> Html Msg
detailedItemView word =
    div
        [ style
            [ ( "height", "100%" )
            , ( "width", "100%" )
            , ( "display", "inline-block" )
            , ( "color", "#eee" )
            , ( "background-color"
              , if word.attested then
                    "blue"
                else
                    "#999"
              )
            ]
          --        , onClick (Attesting <| Save <| word.kana)
        ]
        [ text word.kana ]


itemView : Word -> Html Msg
itemView word =
    div
        [ style
            [ ( "height", "100%" )
            , ( "width", "100%" )
            , ( "display", "inline-block" )
            , ( "color", "#eee" )
            , ( "background-color"
              , if word.attested then
                    "blue"
                else
                    "#999"
              )
            ]
        ]
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


detailedSecondMoraGroupings : ConsonantWiseGrouping Word -> Html Msg
detailedSecondMoraGroupings consonantGroup =
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
                    (List.map detailedItemView vg.items)
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
                            , onClick (PageChange <| Explorer <| Just vg)
                            ]
                            (text vg.dan :: (List.map secondMoraGroupings vg.items))
                    )
                    consonantGroup.items
               )
        )


activeRowView : VowelWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
activeRowView grouping =
    section
        [ class "card" ]
        ((div []
            [ text
                grouping.dan
            ]
         )
            :: (List.map detailedSecondMoraGroupings grouping.items)
        )


gojuonView gojuon maybeActiveRow =
    section [ style [ ( "display", "flex" ) ] ]
        [ (section [ style [ ( "width", "30%" ), ( "display", "inline-block" ) ], classList [ ( "portal", True ) ] ] <|
            List.map firstLevelConsonantView gojuon
          )
        , section [ style [ ( "width", "70%" ), ( "display", "inline-block" ) ] ]
            [ (Maybe.map activeRowView maybeActiveRow)
                |> Maybe.withDefault (text "")
            ]
        ]


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Explorer maybeActiveRow ->
            case model.gojuon of
                Just gojuon ->
                    gojuonView gojuon maybeActiveRow

                Nothing ->
                    text "no data"


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , pageView (model)
        ]
