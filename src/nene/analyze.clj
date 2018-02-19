(ns nene.analyze
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [nene.attesting :as attesting]
            [nene.transliterate :as t :refer [transliterate]]))

;TODO: something about whether certain combinations are impossible -- effectively unpronounceable, or not valid combinations in japanese
;TODO: switch source of truth to database (? - a little harder to work with than the csv file ?)
;TODO: from basic kana pair, like ゴロ, generate -> ゴロゴロ, ゴロンゴロン, and ゴロッと, ごろり
;TODO: something for single-mora based ones, like sotto, zutto, bou-to
;TODO: negative attestations - "this is not a word", or links to evidence that it does kind of exist, like "ginigini"

(defn double-mora [half]
  (str half half))

(defn kana->word [attested-words kana g1 d1 g2 d2]
  (let [word (double-mora kana)
        romaji (transliterate word)]
    {:romaji    romaji
     :kana      word
     :location [g1 d1 g2 d2]
     :attestation (if (attesting/attested-in? attested-words word) "dictionary-word" "unattested")
     }
    )
  )

(defn yoon-map [f]
  (map
    f
    ["ゃ" "ゅ" "ょ"]
    )
  )


(defn k1k2->items [attested-words k1 k2 g1 d1 g2 d2]
  (if (every? some? [k1 k2])
    [(kana->word attested-words (str k1 k2) g1 d1 g2 d2)]
    []
    )
  )

(defn with-k1 [attested-words k1 g1 d1]
  (map
    (fn [c2 g2] {:consonant c2
                :gyo       g2
                :items     (vec (concat (map
                                          (fn [v2 d2]
                                            {:vowel v2
                                             :dan   d2
                                             :items (k1k2->items attested-words k1 (t/get-kana c2 v2) g1 d1 g2 d2)
                                             })
                                          ["a" "i" "u" "e" "o"]
                                          ["ア段" "イ段" "ウ段" "エ段" "オ段"]
                                          )
                                        (if-not (#{"" "w" "y" "nn"} c2) (yoon-map (fn [y] {
                                                                             :vowel y
                                                                             :dan   y
                                                                             :items (k1k2->items attested-words k1 (str (t/get-kana c2 "i") y) g1 d1 g2 y)
                                                                             })
                                                                    ) [])))
                }
      )
    ["", "k", "s", "t", "n", "h", "m", "y", "r", "w", "g", "z", "d", "b", "p" "nn"]
    ["ア行", "カ行", "サ行", "タ行", "ナ行", "ハ行", "マ行", "ヤ行", "ラ行", "ワ行", "ガ行", "ザ行", "ダ行", "バ行", "パ行", "ン行"]
    )
  )


(defn variants []
  (let [attested-words (nene.attesting/get-words)]
    (vec
      (map
        (fn [c g] {
                   :consonant c
                   :gyo       g
                   :items     (vec (concat (map
                                             (fn [v d] {
                                                        :vowel v
                                                        :dan   d
                                                        :items (vec (with-k1 attested-words (t/get-kana c v) g d))
                                                        })
                                             ["a" "i" "u" "e" "o"]
                                             ["ア段" "イ段" "ウ段" "エ段" "オ段"]
                                             )
                                           (if-not (#{"" "w" "y" "nn"} c) (yoon-map

                                                              (fn [y] {
                                                                       :vowel y
                                                                       :dan   y
                                                                       :items (vec (with-k1 attested-words (str (t/get-kana c "i") y) g y))
                                                                       })
                                                              ) [])))
                   }
          )
        ["", "k", "s", "t", "n", "h", "m", "y", "r", "w", "g", "z", "d", "b", "p"]
        ["ア行", "カ行", "サ行", "タ行", "ナ行", "ハ行", "マ行", "ヤ行", "ラ行", "ワ行", "ガ行", "ザ行", "ダ行", "バ行", "パ行"]
        )
      )
    )
  )