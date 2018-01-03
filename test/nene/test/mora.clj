(ns nene.test.mora
  (:use clojure.test
        ring.mock.request
        nene.models.mora))

(deftest test-mora
  (testing "transliteration"
      (is (= (transliterate "ぽつぽつ") "potsupotsu"))
      (is (= (transliterate "ぽったり") "pottari"))
      (is (= (transliterate "びしょびしょ") "bishobisho"))
      (is (= (transliterate "じゃぶじゃぶ") "jabujabu"))
      (is (= (transliterate "きゅんきゅん") "kyunkyun"))
      )

  (testing "char transliteration"
    (is (= (transliterate-char \ぽ) "po"))
    )

  )
