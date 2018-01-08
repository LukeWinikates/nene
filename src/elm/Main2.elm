module Main exposing (..)

import Html exposing (Html, article, button, div, em, header, section, span, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Dict
import Json.Decode as Decode exposing (field)


main =
    Html.program
        { view = view
        , update = update
        , init = ( emptyModel, Http.send GroupFetch (Http.get "/api/words" groupDecoder) )
        , subscriptions = always Sub.none
        }


type Page
    = Universe


type alias Model =
    { notificationText : Maybe String
    , groups : Maybe (List Group)
    , page : Page
    }


emptyModel : Model
emptyModel =
    { notificationText = Nothing
    , groups = Nothing
    , page = Universe
    }


type Msg
    = GroupFetch (Result Http.Error (List Group))
    | PageChange Page


update msg model =
    (case msg of
        GroupFetch (Err er) ->
            { model | notificationText = Just <| toString er } |> noCommand

        GroupFetch (Ok categories) ->
            { model | notificationText = Nothing, groups = Just categories } |> noCommand

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


groupDecoder : Decode.Decoder (List Group)
groupDecoder =
    Decode.list <|
        Decode.map3 Group
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


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Universe ->
            Maybe.map (\groups -> section [] (List.map groupView groups)) model.groups
                |> Maybe.withDefault (text "no groups")


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , pageView (model)
        ]
