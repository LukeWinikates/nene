(ns nene.models.mora
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]))

; todo: switch to a representation of the kana that's laid out like the traditional a/sa/ka/ta chart from the text books
; todo: move the canonical format into csv rather than having the words in this file.
(def transliteration
  {
   "ぽ" "po"
   "ば" "ba"
   "び" "bi"
   "ぶ" "bu"
   "つ" "tsu"
   "ち" "chi"
   "せ" "se"
   "か" "ka"
   "き" "ki"
   "が" "ga"
   "ご" "go"
   "ら" "ra"
   "り" "ri"
   "ろ" "ro"
   "ど" "do"
   "ん" "n"
   "ず" "zu"
   "た" "ta"
   }
  )

(defn transliterate-char [char]
  (get transliteration (str char)))

;todo: this looks awful
(defn transliterate [word]
  (let [fst (first word)
        snd (first (rest word))]
    (match [fst snd]
           [\っ snd] (let [t (transliterate-char snd)]
                      (str (transliterate-char fst) (str (first t) t)
                           (transliterate (str (rest (rest word))))))
           [\し \ゃ] (str "sha" (transliterate (rest (rest word))))
           [\し \ゅ] (str "shu" (transliterate (rest (rest word))))
           [\し \ょ] (str "sho" (transliterate (rest (rest word))))
           [\じ \ゃ] (str "ja" (transliterate (rest (rest word))))
           [\じ \ゅ] (str "ju" (transliterate (rest (rest word))))
           [\じ \ょ] (str "jo" (transliterate (rest (rest word))))
           [fst \ゃ] (str (first (transliterate-char fst)) "ya" (transliterate (rest (rest word))))
           [fst \ゅ] (str (first (transliterate-char fst)) "yu" (transliterate (rest (rest word))))
           [fst \ょ] (str (first (transliterate-char fst)) "yo" (transliterate (rest (rest word))))
           [nil nil] nil
           :else (str (transliterate-char fst) (transliterate (rest word))))
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