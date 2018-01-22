module Main exposing (..)

import Html exposing (Html, article, button, div, em, header, input, section, span, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Dict
import Json.Decode as Decode exposing (field)
import Maybe exposing (withDefault)


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
    | Add (Maybe String)


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


type WordEvent
    = Change String
    | Save String
    | SaveComplete (Result Http.Error Bool)


type Msg
    = GroupFetch (Result Http.Error GitaigoByGojuonOrder)
    | PageChange Page
    | Adder WordEvent


update msg model =
    (case msg of
        GroupFetch (Err er) ->
            { model | notificationText = Just <| toString er } |> noCommand

        GroupFetch (Ok categories) ->
            { model | notificationText = Nothing, gojuon = Just categories } |> noCommand

        PageChange page ->
            { model | page = page } |> clearNotification |> noCommand

        Adder wordEvent ->
            case wordEvent of
                Change word ->
                    { model | page = Add <| Just word } |> clearNotification |> noCommand

                Save word ->
                    ( { model | page = Add Nothing } |> clearNotification, saveWord word )

                SaveComplete (Err error) ->
                    { model | notificationText = Just (toString error) } |> noCommand

                SaveComplete (Ok _) ->
                    { model | page = Add Nothing } |> noCommand
    )


clearNotification : Model -> Model
clearNotification model =
    { model | notificationText = Nothing }


noCommand : Model -> ( Model, Cmd Msg )
noCommand model =
    ( model, Cmd.none )


saveWord : String -> Cmd Msg
saveWord word =
    Http.send
        (SaveComplete >> Adder)
        (Http.post ("/api/words/" ++ word ++ "/attest")
            Http.emptyBody
            (Decode.succeed True)
        )


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


secondLevelConsonantView : ConsonantWiseGrouping Word -> Html Msg
secondLevelConsonantView consonantGroup =
    section [ style [ ( "width", "100%" ) ] ] <|
        (div [ style [] ] [ (text consonantGroup.gyo) ])
            :: List.map
                (\vg ->
                    div [ style [ ( "height", "20px" ), ( "width", "20px" ), ( "border", "1px solid blue" ) ] ] []
                )
                consonantGroup.items


secondMoraGroupings : ConsonantWiseGrouping Word -> Html Msg
secondMoraGroupings consonantGroup =
    section []
        (List.map
            (\vg ->
                div
                    [ style
                        [ ( "height", "38px" )
                        , ( "width", "38px" )
                        , ( "display", "inline-block" )
                        , ( "border", "1px solid blue" )
                        , ( "font-size", "8px" )
                        ]
                    ]
                    (List.map (.romaji >> text) vg.items)
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
                                , ( "width", "196px" )
                                , ( "text-align", "center" )
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


adderView maybeWord =
    (Html.node "form") [ onSubmit (Adder <| Save <| withDefault "" <| maybeWord) ]
        [ input [ onInput <| (Change >> Adder), value <| withDefault "" <| maybeWord ] []
        , button [ type_ "submit" ] [ text "add" ]
        , button [ type_ "button", onClick (PageChange Universe) ] [ text "x" ]
        ]


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Universe ->
            case model.gojuon of
                Just gojuon ->
                    gojuonView gojuon

                Nothing ->
                    text "no data"

        Add maybeWord ->
            adderView maybeWord


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , button [ onClick (PageChange (Add Nothing)) ] [ text "+" ]
        , pageView (model)
        ]
