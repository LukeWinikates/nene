(ns nene.analyze
  (:require [nene.attesting :as attesting]
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

(def all-dan
  (let [simple-strategy (fn [c d] (t/get-kana c (:vowel d)))
        yoon-strategy (fn [c d] (str (t/get-kana c "i") (:dan d)))]
    [
     {:vowel "a" :dan "ア段" :strategy simple-strategy}
     {:vowel "i" :dan "イ段" :strategy simple-strategy}
     {:vowel "u" :dan "ウ段" :strategy simple-strategy}
     {:vowel "e" :dan "エ段" :strategy simple-strategy}
     {:vowel "o" :dan "オ段" :strategy simple-strategy}
     {:vowel "ya" :dan "ゃ" :strategy yoon-strategy}
     {:vowel "yu" :dan "ゅ" :strategy yoon-strategy}
     {:vowel "yo" :dan "ょ" :strategy yoon-strategy}
     ]
    )
  )

(defn map-vowel [consonant f]
  (->>
    all-dan
    (take (if (#{"" "w" "y" "nn"} consonant) 5 8))
    (map
      (fn [dan] (f dan ((get dan :strategy) consonant dan))))
    )
  )


(defn with-k1 [attested-words k1 g1 d1]
  (map
    (fn [{c2 :consonant g2 :gyo}]
      {:consonant c2
       :gyo       g2
       :items     (vec
                    (map-vowel
                      c2
                      (fn [{v2 :vowel d2 :dan} k2]
                        {:vowel v2
                         :dan   d2
                         :items (k1k2->items attested-words k1 k2 g1 d1 g2 d2)
                         }
                        )
                      )
                    )
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
           :items     (vec
                        (map-vowel
                          c
                          (fn [{v :vowel d :dan} k1]
                            {
                             :vowel v
                             :dan   d
                             :items (vec (with-k1 attested-words k1 g d))
                             }
                            )
                          ))
           }
          )
        gyo-except-last
        )
      )
    )
  )