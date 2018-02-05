module Main exposing (..)

import Html exposing (Html, article, button, div, em, header, input, section, span, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Dict
import Json.Decode as Decode exposing (field)
import Maybe exposing (withDefault)


main =
    Html.program
        { view = view
        , update = update
        , init = ( emptyModel, Http.send LoadGojuonGrid (Http.get "/api/words" gojuonDecoder) )
        , subscriptions = always Sub.none
        }


type Page
    = Explorer


type alias Model =
    { notificationText : Maybe String
    , gojuon : Maybe GitaigoByGojuonOrder
    , page : Page
    }


emptyModel : Model
emptyModel =
    { notificationText = Nothing
    , gojuon = Nothing
    , page = Explorer
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
            { model | notificationText = Just <| toString er } |> noCommand

        LoadGojuonGrid (Ok categories) ->
            { model | notificationText = Nothing, gojuon = Just categories } |> noCommand

        PageChange page ->
            { model | page = page } |> clearNotification |> noCommand

        Attesting e ->
            case e of
                Save word ->
                    ( { model | page = Explorer } |> clearNotification, saveWord word )

                -- TODO: don't reload the whole grid here
                SaveComplete (Err error) ->
                    { model | notificationText = Just (toString error) }
                        |> withCommand (Http.send LoadGojuonGrid (Http.get "/api/words" gojuonDecoder))

                SaveComplete (Ok _) ->
                    model
                        |> withCommand (Http.send LoadGojuonGrid (Http.get "/api/words" gojuonDecoder))
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


saveWord : String -> Cmd Msg
saveWord word =
    Http.send
        (SaveComplete >> Attesting)
        (Http.post ("/api/words/" ++ word ++ "/attest")
            Http.emptyBody
            (Decode.succeed True)
        )


type alias Word =
    { kana : String
    , romaji : String
    , attested : Bool
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
    , gyo : String
    , items : List (VowelWiseGrouping a)
    }


type alias VowelWiseGrouping a =
    { vowel : String
    , dan : String
    , items : List a
    }


validInitialConsonants =
    [ "", "k", "s", "t", "n", "h", "m", "y", "r", "w", "g", "z", "d", "b", "p" ]


vowelWiseGroupingDecoder : Decode.Decoder a -> Decode.Decoder (VowelWiseGrouping a)
vowelWiseGroupingDecoder decoder =
    Decode.map3 VowelWiseGrouping
        (field "vowel" Decode.string)
        (field "dan" Decode.string)
        (field "items" (Decode.list decoder))


consonantWiseGroupingDecoder : Decode.Decoder a -> Decode.Decoder (ConsonantWiseGrouping a)
consonantWiseGroupingDecoder decoder =
    Decode.map3 ConsonantWiseGrouping
        (field "consonant" Decode.string)
        (field "gyo" Decode.string)
        (field "items" (Decode.list (vowelWiseGroupingDecoder decoder)))


gojuonDecoder : Decode.Decoder GitaigoByGojuonOrder
gojuonDecoder =
    Decode.list
        (consonantWiseGroupingDecoder (consonantWiseGroupingDecoder wordDecoder))


wordDecoder : Decode.Decoder Word
wordDecoder =
    Decode.map3 Word
        (field "kana" Decode.string)
        (field "romaji" Decode.string)
        (field "attested?" Decode.bool)


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
        , onClick (Attesting <| Save <| word.kana)
        ]
        []


secondMoraGroupings : ConsonantWiseGrouping Word -> Html Msg
secondMoraGroupings consonantGroup =
    section []
        (List.map
            (\vg ->
                div
                    [ style
                        [ ( "height", "15px" )
                        , ( "width", "15px" )
                        , ( "display", "inline-block" )
                        , ( "font-size", "8px" )
                        ]
                    ]
                    (List.map itemView vg.items)
            )
            consonantGroup.items
        )


firstLevelConsonantView : ConsonantWiseGrouping (ConsonantWiseGrouping Word) -> Html Msg
firstLevelConsonantView consonantGroup =
    section [ style [ ( "border-bottom", "1px solid gray" ) ] ]
        (div [ style [] ] [ (text consonantGroup.gyo) ]
            :: (List.map
                    (\vg ->
                        div
                            [ style
                                [ ( "display", "inline-block" )
                                , ( "width", "122px" )
                                ]
                            ]
                            (text vg.dan :: (List.map secondMoraGroupings vg.items))
                    )
                    consonantGroup.items
               )
        )


gojuonView gojuon =
    section [] <|
        List.map firstLevelConsonantView gojuon


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Explorer ->
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
