(ns nene.transliterate
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]))

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

(def hiragana-vector
  (vec (apply concat (vals hiragana))))

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
      (let [idx (index-of hiragana-vector kana)
            gyo (int (/ idx 5))
            keta (mod idx 5)
            naive (str (nth (keys hiragana) gyo) (nth "aiueo" keta))]
        naive)
      ))
  )

;todo: this still doesn't look right
(defn transliterate-naive [word]
  (cond
    (= [] word) ""
    (= (first word) \っ)
    (let [t (transliterate-single-kana (first (rest word)))]
      (str (first t)
           (transliterate-naive (rest word))))
    (#{\ゃ \ゅ \ょ} (first (rest word)))
    (match [(first word) (first (rest word))]
           [fst \ゃ] (str (first (transliterate-single-kana fst)) "ya" (transliterate-naive (rest (rest word))))
           [fst \ゅ] (str (first (transliterate-single-kana fst)) "yu" (transliterate-naive (rest (rest word))))
           [fst \ょ] (str (first (transliterate-single-kana fst)) "yo" (transliterate-naive (rest (rest word))))
           )
    :else
    (str (transliterate-single-kana (first word)) (transliterate-naive (rest word)))))

(defn transliterate [word]
  (let [replacements [["hu" "fu"]
                      ["si" "shi"]
                      ["zi" "ji"]
                      ["ti" "chi"]
                      ["di" "ji"]
                      ["tu" "tsu"]
                      ["du" "dzu"]
                      ["sy" "sh"]
                      ["ty" "ch"]
                      ["dy" "j"]
                      ["zy" "j"]
                      ]]
    (reduce
      (fn [val [m r]]
        (string/replace val m r))
      (transliterate-naive word)
      replacements)))