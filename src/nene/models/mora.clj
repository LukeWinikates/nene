(ns nene.models.mora)

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

(defn word-structures [word]
  (-> word
      transliterate)
  )

(def words
  (->> ["ぽつぽつ" "せかせか" "どんどん" "ずたずた" "ばらばら" "がらがら" "がちがち" "ごろんごろん"]
       (map (fn [word] {:kana word :romaji (transliterate word)}))))
