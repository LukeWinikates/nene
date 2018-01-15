(ns nene.analyze
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [nene.transliterate :as t :refer [transliterate]]))


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
  (remove #{"ん"} (remove nil? t/hiragana-vector)))

(defn double-mora [half]
  (str half half))

(defn kana->kana-romaji-map [word]
  {:kana word :romaji (transliterate word)})

(defn variants []
  (->> valid-first-mora
       (map
         (fn [first-mora]
           {
            :en    (transliterate first-mora)
            :jp    (str first-mora)
            :words (map
                     (comp kana->kana-romaji-map double-mora)
                     (concat (map
                               (fn [second-mora] (str first-mora second-mora))
                               (remove nil? t/hiragana-vector))
                             (map
                               (fn [second-mora] (str first-mora second-mora "ん"))
                               valid-first-mora)))
            }

           ))))