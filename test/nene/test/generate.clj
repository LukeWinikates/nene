(ns nene.test.generate
  (:require [clojure.test :refer :all]
            [nene.analyze :as a]
            [nene.test.regression :refer [regression-variants]]))
(defn just-items [v]
  (->> v
       (map #(get-in % [:items]))
       (flatten)
       (map #(get-in % [:items]))
       (flatten)
       (map #(get-in % [:items]))
       (flatten)
       (map #(get-in % [:items]))
       (flatten)
       (map :kana)
       ))

(defn gyo [variants g]
  (filter #(= (:gyo %) g) variants))

(deftest test-generating
  (testing "variants"
    (is (some #{"ぽつぽつ"} (just-items (a/variants))))
    (is (some #{"きょろきょろ"} (just-items (a/variants))))
    (is (some #{"もじゃもじゃ"} (just-items (a/variants))))
    )
  )


