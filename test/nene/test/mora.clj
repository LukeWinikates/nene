(ns nene.test.mora
  (:use clojure.test
        ring.mock.request
        nene.models.mora))

(deftest test-mora
  (testing "transliteration"
      (is (= (transliterate "ぽつぽつ") "potsupotsu"))))
