(ns nene.analyze
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [nene.attesting :as attesting]
            [nene.transliterate :as t :refer [transliterate]]))

;TODO: something about whether certain combinations are impossible -- effectively unpronounceable, or not valid combinations in japanese
;TODO: clean up unused/dead functions and routes
;TODO: switch source of truth to database (? - a little harder to work with than the csv file ?)
;TODO: from basic kana pair, like ゴロ, generate -> ゴロゴロ, ゴロンゴロン, and ゴロッと, ごろり
;TODO: something for single-mora based ones, like sotto, zutto, bou-to
;TODO: negative attestations - "this is not a word", or links to evidence that it does kind of exist, like "ginigini"
(defn second-morae [kana]
  (string/join (rest (take (/ (count kana) 2) kana)))
  )

(defn analyze [{:keys [kana] :as word}]
  (-> word
      (assoc :attributes [
                          {:label (str (first kana) "_" (first kana) "_") :flavor :first-mora}
                          {:label (str "_" (second-morae kana) "_" (second-morae kana)) :flavor :second-morae}
                          ]
             )
      )
  )

(def words
  (->> (with-open [reader (io/reader "resources/words.csv")]
         (doall
           (->> (csv/read-csv reader)
                (map first))))
       (map (fn [word] {:kana word :romaji (transliterate word)}))
       (map analyze)
       )
  )
; todo: remove this variant
(defn attested? [romaji]
  (boolean (not-empty (filter #(= (:romaji %) romaji) words))))

(defn matches-attribute? [group-kana word]
  (some
    (fn [attr] (= (:label attr) group-kana))
    (:attributes word)
    )
  )

(defn words-for-group [group-kana]
  (filter (partial matches-attribute? group-kana) words)
  )

(defn find-word-by-romaji [romaji]
  (first (filter #(= romaji (:romaji %)) words)))

(defn relatives [word]
  (assoc
    word
    :relatives
    (map (fn [attribute]
           {:label (:label attribute)
            :words (map :kana (filter #(not (= word %)) (words-for-group (:label attribute))))
            }
           ) (:attributes word)))
  )

(defn double-mora [half]
  (str half half))

(defn kana->word [kana]
  (let [word (double-mora kana)
        romaji (transliterate word)]
    {:romaji    romaji
     :kana      word
     :attested? (or (attested? romaji)
                    (attesting/attested-in? (nene.attesting/get-words) romaji))
     }
    )
  )

;todo: find a way of refactoring it so that the item generation is abstracted out and all the variables are in scope all the time...
;todo: dropping information along the way seems to be part of the pain?
; todo solve for romaji representation of the vowel here
(defn yoon-map [f]
  (map
    f
    ["ゃ" "ゅ" "ょ"]
    )
  )


(defn k1k2->items [k1 k2]
  (if (every? some? [k1 k2])
    (vec (map kana->word [(str k1 k2)]))
    []
    ))

(defn with-k1 [k1]
  (map
    (fn [c2 g] {:consonant c2
                :gyo       g
                :items     (vec (concat (map
                                          (fn [v2 d]
                                            {:vowel v2
                                             :dan   d
                                             :items (k1k2->items k1 (t/get-kana c2 v2))
                                             })
                                          ["a" "i" "u" "e" "o"]
                                          ["ア段" "イ段" "ウ段" "エ段" "オ段"]
                                          )
                                        (if-not (= "" c2) (yoon-map (fn [y] {
                                                                             :vowel y
                                                                             :dan   y
                                                                             :items (k1k2->items k1 (str (t/get-kana c2 "i") y))
                                                                             })
                                                                    ) [])))
                }
      )
    ["", "k", "s", "t", "n", "h", "m", "y", "r", "w", "g", "z", "d", "b", "p" "nn"]
    ["ア行", "カ行", "サ行", "タ行", "ナ行", "ハ行", "マ行", "ヤ行", "ラ行", "ワ行", "ガ行", "ザ行", "ダ行", "バ行", "パ行", "ン行"]
    )
  )


(defn variants []
  (vec
    (map
      (fn [c g] {
                 :consonant c
                 :gyo       g
                 :items     (vec (concat (map
                                           (fn [v d] {
                                                      :vowel v
                                                      :dan   d
                                                      :items (vec (with-k1 (t/get-kana c v)))
                                                      })
                                           ["a" "i" "u" "e" "o"]
                                           ["ア段" "イ段" "ウ段" "エ段" "オ段"]
                                           )
                                         (if-not (= "" c) (yoon-map

                                                            (fn [y] {
                                                                     :vowel y
                                                                     :dan   y
                                                                     :items (vec (with-k1 (str (t/get-kana c "i") y)))
                                                                     })
                                                            ) [])))
                 }
        )
      ["", "k", "s", "t", "n", "h", "m", "y", "r", "w", "g", "z", "d", "b", "p"]
      ["ア行", "カ行", "サ行", "タ行", "ナ行", "ハ行", "マ行", "ヤ行", "ラ行", "ワ行", "ガ行", "ザ行", "ダ行", "バ行", "パ行"]
      )))