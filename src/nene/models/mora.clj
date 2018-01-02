(ns nene.models.mora
  (:require [clojure.string :as string]))

(def transliteration
  {
   "ぽ" "po"
   "ば" "ba"
   "つ" "tsu"
   "ち" "chi"
   "せ" "se"
   "か" "ka"
   "が" "ga"
   "ご" "go"
   "ら" "ra"
   "ろ" "ro"
   "ど" "do"
   "ん" "n"
   "ず" "zu"
   "た" "ta"
   }
  )

(defn transliterate [word]
  (clojure.string/join (map #(get transliteration (str %)) word)))

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

;todo: support ちゃらちゃら/じゃらじゃら
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