(ns nene.analyze
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [nene.attesting :as attesting]
            [nene.transliterate :as t :refer [transliterate]]))

;TODO: try enums again
;TODO: something about whether certain combinations are impossible -- effectively unpronounceable, or not valid combinations in japanese
;TODO: from basic kana pair, like ゴロ, generate -> ゴロゴロ, ゴロンゴロン, and ゴロッと, ごろり
;TODO: something for single-mora based ones, like sotto, zutto, bou-to
;TODO: negative attestations - "this is not a word", or links to evidence that it does kind of exist, like "ginigini"

(defn double-mora [half]
  (str half half))

(defn kana->word [attested-words kana g1 d1 g2 d2]
  (let [word (double-mora kana)
        romaji (transliterate word)]
    {:romaji      romaji
     :kana        word
     :location    [g1 d1 g2 d2]
     :attestation (attesting/attestation-for attested-words word)
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

(def all-gyo
  [
   {:consonant "" :gyo "ア行"}
   {:consonant "k" :gyo "カ行"}
   {:consonant "g" :gyo "ガ行"}
   {:consonant "s" :gyo "サ行"}
   {:consonant "z" :gyo "ザ行"}
   {:consonant "t" :gyo "タ行"}
   {:consonant "d" :gyo "ダ行"}
   {:consonant "n" :gyo "ナ行"}
   {:consonant "h" :gyo "ハ行"}
   {:consonant "b" :gyo "バ行"}
   {:consonant "p" :gyo "パ行"}
   {:consonant "m" :gyo "マ行"}
   {:consonant "y" :gyo "ヤ行"}
   {:consonant "r" :gyo "ラ行"}
   {:consonant "w" :gyo "ワ行"}
   {:consonant "nn" :gyo "ン行"}
   ]
  )

(def gyo-except-last
  (reverse (drop 1 (reverse all-gyo))))

(defn with-k1 [attested-words k1 g1 d1]
  (map
    (fn [{c2 :consonant g2 :gyo}]
      {:consonant c2
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
    all-gyo
    )
  )


(defn variants []
  (let [attested-words (nene.attesting/get-words)]
    (vec
      (map
        (fn [{c :consonant g :gyo}]
          {
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
        gyo-except-last
        )
      )
    )
  )