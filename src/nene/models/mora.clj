(ns nene.models.mora
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]))

; todo: move the canonical format into csv rather than having the words in this file.

(def hiragana
  (array-map
    "" ["あ" "い" "う" "え" "お"]
    "k" ["か" "き" "く" "け" "こ"]
    "s" ["さ" "し" "す" "せ" "そ"]
    "t" ["た" "ち" "つ" "て" "と"]
    "n" ["な" "に" "ぬ" "ね" "の"]
    "h" ["は" "ひ" "ふ" "へ" "ほ"]
    "m" ["ま" "み" "む" "め" "も"]
    "y" ["や" nil "ゆ" nil "よ"]
    "r" ["ら" "り" "る" "れ" "ろ"]
    "w" ["わ" nil nil nil "を"]
    "g" ["が" "ぎ" "ぐ" "げ" "ご"]
    "z" ["ざ" "じ" "ず" "ぜ" "ぞ"]
    "d" ["だ" "ぢ" "づ" "で" "ど"]
    "b" ["ば" "び" "ぶ" "べ" "ぼ"]
    "p" ["ぱ" "ぴ" "ぷ" "ぺ" "ぽ"]
    "nn" ["ん"]
    )
  )

(defn index-of [coll item]
  (->> (map-indexed vector coll)
       (filter (fn [[_ el]] (= item el)))
       (first)
       (first)))

(defn transliterate-single-kana [kana]
  (let [kana (str kana)]
    (case kana
      "ん" "n"
      "っ" nil
      (let [hiragana-vector (vec (apply concat (vals hiragana)))
            gyo (int (/ (index-of hiragana-vector kana) 5))
            keta (mod (index-of hiragana-vector kana) 5)
            naive (str (nth (keys hiragana) gyo) (nth "aiueo" keta))]
        (case naive
          "si" "shi"
          "zi" "ji"
          "ti" "chi"
          "di" "ji"
          "tu" "tsu"
          "du" "dzu"
          naive)
        )
      ))
  )

;todo: this looks awful
(defn transliterate [word]
  (let [fst (first word)
        snd (first (rest word))]
    (match [fst snd]
           [\っ snd] (let [t (transliterate-single-kana snd)]
                      (str (transliterate-single-kana fst) (str (first t) t)
                           (transliterate (rest (rest word)))))
           [\し \ゃ] (str "sha" (transliterate (rest (rest word))))
           [\し \ゅ] (str "shu" (transliterate (rest (rest word))))
           [\し \ょ] (str "sho" (transliterate (rest (rest word))))
           [\じ \ゃ] (str "ja" (transliterate (rest (rest word))))
           [\じ \ゅ] (str "ju" (transliterate (rest (rest word))))
           [\じ \ょ] (str "jo" (transliterate (rest (rest word))))
           [fst \ゃ] (str (first (transliterate-single-kana fst)) "ya" (transliterate (rest (rest word))))
           [fst \ゅ] (str (first (transliterate-single-kana fst)) "yu" (transliterate (rest (rest word))))
           [fst \ょ] (str (first (transliterate-single-kana fst)) "yo" (transliterate (rest (rest word))))
           [nil nil] nil
           :else (str (transliterate-single-kana fst) (transliterate (rest word))))
    ))

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
  (->> ["ぽつぽつ" "せかせか" "どんどん" "ずたずた" "ばらばら" "がらがら" "がちがち" "ごろんごろん" "はらはら" "ちらちら" "きらきら"]
       (map (fn [word] {:kana word :romaji (transliterate word)}))
       (map analyze)
       )
  )

; for each word
; if it has an attribute matching this one
; include the word by its kana representation

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