(ns nene.models.mora
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]))

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
  (remove #{"ん"} (remove nil? hiragana-vector)))

(defn doubleify [half]
  (str half half))

(defn wordup [word]
  {:kana word :romaji (transliterate word)})

(defn variants []
  (->> valid-first-mora
       (map
         (fn [first-mora]
           {
            :en    (transliterate first-mora)
            :jp    (str first-mora)
            :words (map (comp wordup doubleify) (concat (map (fn [second-mora]
                                                 (str first-mora second-mora)
                                                 )
                                               (remove nil? hiragana-vector))
                                          (map (fn [second-mora]
                                                 (str first-mora second-mora "ん")
                                                 )
                                               valid-first-mora)))
            }

           ))))