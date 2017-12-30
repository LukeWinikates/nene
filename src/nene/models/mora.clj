(ns nene.models.mora)

(def words ["ぽつぽつ" "せかせか" "どんどん" "ずたずた"])

(def transliteration
  {
   "ぽ" "po"
   "つ" "tsu"
   "せ" "se"
   "か" "ka"
   "ど" "do"
   "ん" "n"
   "ず" "zu"
   "た" "ta"
   }
  )

(defn transliterate [word]
  (clojure.string/join (map #(get transliteration (str %)) word)))

(defn word-structures [word]
  (-> word
      transliterate)
  )
