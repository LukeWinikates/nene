(ns nene.analyze
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [nene.transliterate :as t :refer [transliterate]]))

;TODO: something about whether certain combinations are impossible -- effectively unpronounceable, or not valid combinations in japanese
;TODO: clean up unused/dead functions and routes
;TODO: switch source of truth to database (? - a little harder to work with than the csv file ?)
;TODO: from basic kana pair, like ゴロ, generate -> ゴロゴロ, ゴロンゴロン, and ゴロッと, ごろり
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

(def valid-first-mora
  (remove #{"ん"} (remove nil? t/hiragana-vector)))

(defn double-mora [half]
  (str half half))

(defn kana->kana-romaji-map [word]
  {:kana word :romaji (transliterate word)})

(defn romaji->word [romaji]
  {:romaji    (double-mora romaji)
   :kana      ""
   :attested? (attested? (double-mora romaji))})

; todo: switch this to generate using the kana instead of the romaji
(defn with-cvc [cv c2]
  (map
    (fn [v2 d] {:vowel v2
                :dan   d
                :items (vec (map romaji->word [(str cv c2 v2)]))})
      ["a" "i" "u" "e" "o"]
      ["ア段" "イ段" "ウ段" "エ段" "オ段"]
    )
  )

;todo: figure out something smart with the mismatch between the consonant lists here, then 'nn' is not nice
(defn with-first-mora [cv]
  (map
    (fn [c g] {:consonant c :gyo g :items (vec (with-cvc cv c))})
    ["", "k", "s", "t", "n", "h", "m", "y", "r", "w", "g", "z", "d", "b", "p" "nn"]
    ["ア行", "カ行", "サ行", "タ行", "ナ行", "ハ行", "マ行", "ヤ行", "ラ行", "ワ行", "ガ行", "ザ行", "ダ行", "バ行", "パ行", "ン行"]
    )
  )

(defn with-starting-consonant [c]
  (map
    (fn [v d] {:vowel v :dan d :items (vec (with-first-mora (str c v)))})
    ["a" "i" "u" "e" "o"]
    ["ア段" "イ段" "ウ段" "エ段" "オ段"]
    )
  )

(defn variants []
  (vec
    (map
      (fn [c g] {:consonant c :gyo g :items (with-starting-consonant c)})
      ["", "k", "s", "t", "n", "h", "m", "y", "r", "w", "g", "z", "d", "b", "p"]
      ["ア行", "カ行", "サ行", "タ行", "ナ行", "ハ行", "マ行", "ヤ行", "ラ行", "ワ行", "ガ行", "ザ行", "ダ行", "バ行", "パ行"]
      )))