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
                          { :label (str (first kana) "_" (first kana) "_") :flavor :first-mora }
                          { :label (str "_" (second-morae kana) "_" (second-morae kana)) :flavor :second-morae }
                          ]
             )
      )
  )



(def words
  (->> ["ぽつぽつ" "せかせか" "どんどん" "ずたずた" "ばらばら" "がらがら" "がちがち" "ごろんごろん"]
       (map (fn [word] {:kana word :romaji (transliterate word)}))
       (map analyze)
       )
  )
