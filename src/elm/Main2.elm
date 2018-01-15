module Main exposing (..)

import Html exposing (Html, article, button, div, em, header, section, span, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Dict
import Json.Decode as Decode exposing (field)

--todo: finish renaming GroupFetch to something else... there gould be a sort of generic fetching type
--todo: provide kana representations for each consonant/vowel 行・段
main =
    Html.program
        { view = view
        , update = update
        , init = ( emptyModel, Http.send GroupFetch (Http.get "/api/words" universeDecoder) )
        , subscriptions = always Sub.none
        }


type Page
    = Universe


type alias Model =
    { notificationText : Maybe String
    , gojuon : Maybe GitaigoByGojuonOrder
    , page : Page
    }


emptyModel : Model
emptyModel =
    { notificationText = Nothing
    , gojuon = Nothing
    , page = Universe
    }


type Msg
    = GroupFetch (Result Http.Error GitaigoByGojuonOrder)
    | PageChange Page


update msg model =
    (case msg of
        GroupFetch (Err er) ->
            { model | notificationText = Just <| toString er } |> noCommand

        GroupFetch (Ok categories) ->
            { model | notificationText = Nothing, gojuon = Just categories } |> noCommand

        PageChange page ->
            { model | page = page } |> noCommand
    )


noCommand : Model -> ( Model, Cmd Msg )
noCommand model =
    ( model, Cmd.none )


type alias Word =
    { kana : String
    , romaji : String
    }


type alias Group =
    { en : String
    , jp : String
    , words : List Word
    }


type alias GitaigoByGojuonOrder =
    List (ConsonantWiseGrouping (ConsonantWiseGrouping Word))


type alias ConsonantWiseGrouping a =
    { consonant : String
    , items : List (VowelWiseGrouping a)
    }


type alias VowelWiseGrouping a =
    { vowel : String
    , items : List a
    }


startingValue : GitaigoByGojuonOrder
startingValue =
    [ { consonant = ""
      , items =
            [ { vowel = "a"
              , items =
                    [ { consonant = ""
                      , items =
                            [ { vowel = "a"
                              , items =
                                    [ { romaji = "aa", kana = "" }
                                    , { romaji = "aan", kana = "" }
                                    ]
                              }
                            ]
                      }
                    ]
              }
            ]
      }
    ]


validInitialConsonants =
    [ "", "k", "s", "t", "n", "h", "m", "y", "r", "w", "g", "z", "d", "b", "p" ]


vowelWiseGroupingDecoder : Decode.Decoder a -> Decode.Decoder (VowelWiseGrouping a)
vowelWiseGroupingDecoder decoder =
    Decode.map2 VowelWiseGrouping
        (field "vowel" Decode.string)
        (field "items" (Decode.list decoder))


consonantWiseGroupingDecoder : Decode.Decoder a -> Decode.Decoder (ConsonantWiseGrouping a)
consonantWiseGroupingDecoder decoder =
    Decode.map2 ConsonantWiseGrouping
        (field "consonant" Decode.string)
        (field "items" (Decode.list (vowelWiseGroupingDecoder decoder)))


universeDecoder : Decode.Decoder GitaigoByGojuonOrder
universeDecoder =
    Decode.list
        (consonantWiseGroupingDecoder (consonantWiseGroupingDecoder wordDecoder))


wordDecoder : Decode.Decoder Word
wordDecoder =
    Decode.map2 Word
        (field "kana" Decode.string)
        (field "romaji" Decode.string)


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


firstLevelConsonantView : ConsonantWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
firstLevelConsonantView consonantGroup =
    section [] <|
        (text consonantGroup.consonant)
            :: List.map (\vg -> (text vg.vowel)) consonantGroup.items


gojuonView gojuon =
    section [] <|
        List.map firstLevelConsonantView gojuon


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Universe ->
            case model.gojuon of
                Just gojuon ->
                    gojuonView gojuon

                Nothing ->
                    text "no data"


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , pageView (model)
        ]
