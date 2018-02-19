module Ajax exposing (..)

import Http exposing (Request)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode


attestWord : String -> Attestation -> Request Bool
attestWord kana attestation =
    (Http.post ("/api/attestations")
        (Http.jsonBody
            (Encode.object
                [ ( "kana", (Encode.string kana) )
                , ( "attestation-type", (Encode.string <| attestationToString attestation) )
                ]
            )
        )
        (Decode.succeed True)
    )


attestationToString : Attestation -> String
attestationToString attestation =
    case attestation of
        DictionaryWord ->
            "dictionary-word"

        InternetExamples ->
            "internet-examples"

        Unattested ->
            "unattested"

        HardToPronounce ->
            "hard-to-pronounce"

        Unlikely ->
            "unlikely"

        Impossible ->
            "impossible"


type Attestation
    = DictionaryWord
    | InternetExamples
    | Unattested
    | HardToPronounce
    | Unlikely
    | Impossible


type alias Word =
    { kana : String
    , romaji : String
    , attestation : Attestation
    , location : List String
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


stringToAttestation : String -> Maybe Attestation
stringToAttestation s =
    case s of
        "dictionary-word" ->
            Just DictionaryWord

        "internet-examples" ->
            Just InternetExamples

        "unattested" ->
            Just Unattested

        "hard-to-pronounce" ->
            Just HardToPronounce

        "unlikely" ->
            Just Unlikely

        "impossible" ->
            Just Impossible

        _ ->
            Nothing


attestationDecoder : Decode.Decoder Attestation
attestationDecoder =
    Decode.string
        |> (Decode.andThen
                (stringToAttestation
                    >> ((Maybe.map Decode.succeed)
                            >> (Maybe.withDefault (Decode.fail "unrecognized value"))
                       )
                )
           )


wordDecoder : Decode.Decoder Word
wordDecoder =
    Decode.map4 Word
        (field "kana" Decode.string)
        (field "romaji" Decode.string)
        (field "attestation" attestationDecoder)
        (field "location" (Decode.list Decode.string))
