module Main exposing (..)

import Html exposing (Html, article, button, div, em, section, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (field)


main =
    Html.program
        { view = view
        , update = update
        , init = ( emptyModel, Http.send CategoryFetch (Http.get "/api/gigo" categoriesDecoder) )
        , subscriptions = always Sub.none
        }


type alias Model =
    { notificationText : Maybe String
    , categories : Maybe (List Category)
    }


emptyModel : Model
emptyModel =
    { notificationText = Nothing
    , categories = Nothing
    }


type Msg
    = CategoryFetch (Result Http.Error (List Category))


update msg model =
    ( case msg of
        CategoryFetch (Err er) ->
            { model | notificationText = Just <| toString er }

        CategoryFetch (Ok categories) ->
            { model | notificationText = Nothing, categories = Just categories }
    , Cmd.none
    )


type alias Word =
    { kana : String
    , romaji : String
    }


type alias Category =
    { en : String
    , jp : String
    , words : List Word
    }


categoriesDecoder : Decode.Decoder (List Category)
categoriesDecoder =
    Decode.list <|
        Decode.map3 Category
            (field "en" Decode.string)
            (field "jp" Decode.string)
            (field "words" (Decode.list wordDecoder))


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


categoryView : Category -> Html Msg
categoryView category =
    article [ class "card" ]
        [ section [ class "card-label vertical" ]
            [ div [] [ text category.jp ]
            , div [] [ text category.en ]
            ]
        , section [ class "card-content pill-container" ]
            (List.map wordView category.words)
        ]


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , Maybe.map (\categories -> section [] (List.map categoryView categories)) model.categories
            |> Maybe.withDefault (text "no categories")
        ]