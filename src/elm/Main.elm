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
        , init = ( emptyModel, Http.send CategoryFetch (Http.get "/api/gigo" categoriesDecoder) )
        , subscriptions = always Sub.none
        }


type Page
    = CategoryList
    | WordDetail Word


type alias Model =
    { notificationText : Maybe String
    , categories : Maybe (List Category)
    , page : Page
    , relatives : Dict.Dict String (List Relative)
    }


emptyModel : Model
emptyModel =
    { notificationText = Nothing
    , categories = Nothing
    , page = CategoryList
    , relatives = Dict.empty
    }


type Msg
    = CategoryFetch (Result Http.Error (List Category))
    | RelativesFetch Word (Result Http.Error (List Relative))
    | PageChange Page


update msg model =
    (case msg of
        CategoryFetch (Err er) ->
            { model | notificationText = Just <| toString er } |> noCommand

        CategoryFetch (Ok categories) ->
            { model | notificationText = Nothing, categories = Just categories } |> noCommand

        RelativesFetch _ (Err er) ->
            { model | notificationText = Just <| toString er } |> noCommand

        RelativesFetch word (Ok relatives) ->
            (saveRelatives word relatives model) |> noCommand

        PageChange page ->
            { model | page = page } |> pageCommand page
    )


noCommand : Model -> ( Model, Cmd Msg )
noCommand model =
    ( model, Cmd.none )


saveRelatives : Word -> List Relative -> Model -> Model
saveRelatives word relatives model =
    { model | relatives = Dict.insert word.romaji relatives model.relatives }


pageCommand : Page -> Model -> ( Model, Cmd Msg )
pageCommand page model =
    ((,) model) <|
        case page of
            WordDetail word ->
                Http.send (RelativesFetch word) (Http.get ("/api/gigo/" ++ word.romaji ++ "/relatives") relativesDecoder)

            _ ->
                Cmd.none


type Loadable a
    = NotLoaded
    | Loaded a


type alias Relative =
    { label : String
    , words : List String
    }


type alias Word =
    { kana : String
    , romaji : String
    , attributes :
        List WordAttribute
        --    , relatives : Loadable (List Relative)
    }


mapLoadable : Loadable a -> (a -> b) -> Maybe b
mapLoadable loadable f =
    case loadable of
        NotLoaded ->
            Nothing

        Loaded something ->
            Just <| f something


type alias WordAttribute =
    { label : String
    , flavor : String
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


attributeDecoder : Decode.Decoder WordAttribute
attributeDecoder =
    Decode.map2 WordAttribute
        (field "label" Decode.string)
        (field "flavor" Decode.string)


wordDecoder : Decode.Decoder Word
wordDecoder =
    Decode.map3 Word
        (field "kana" Decode.string)
        (field "romaji" Decode.string)
        (field "attributes" (Decode.list attributeDecoder))



--        (Decode.succeed NotLoaded)


relativeDecoder : Decode.Decoder Relative
relativeDecoder =
    Decode.map2 Relative
        (field "label" Decode.string)
        (field "words" (Decode.list Decode.string))


relativesDecoder =
    field "relatives" (Decode.list relativeDecoder)


wordView : Word -> Html Msg
wordView word =
    div [ class "pill", onClick (PageChange <| WordDetail word) ]
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


wordDetailView : Word -> Maybe (List Relative) -> Html Msg
wordDetailView word relatives =
    article [ class "super-rounded" ]
        [ header []
            [ strong [ style [ ( "font-size", "16px" ) ] ] [ text word.kana ]
            , em [] [ text word.romaji ]
            , span [ onClick (PageChange <| CategoryList), style [ ( "float", "right" ) ] ] [ text "x" ]
            ]
        , section []
            (List.map
                (\attr ->
                    div []
                        [ strong [] [ text attr.label ]
                        , span [] [ text ":" ]
                        , (text attr.flavor)
                        ]
                )
                word.attributes
            )
        , section []
            ((Maybe.map
                (\rels ->
                    List.map
                        (\relative ->
                            div []
                                [ strong [] [ text relative.label ]
                                , span [] [ text ":" ]
                                , (text <| String.join ", " relative.words)
                                ]
                        )
                        rels
                )
                relatives
             )
                |> Maybe.withDefault []
            )
        ]


pageView : Model -> Html Msg
pageView model =
    case model.page of
        CategoryList ->
            Maybe.map (\categories -> section [] (List.map categoryView categories)) model.categories
                |> Maybe.withDefault (text "no categories")

        WordDetail word ->
            wordDetailView word (Dict.get word.romaji model.relatives)


view model =
    div []
        [ div [] [ text <| Maybe.withDefault "" model.notificationText ]
        , pageView (model)
        ]
