(ns nene.attesting
  (:require [korma.db :refer [defdb postgres]]
            [korma.core :refer [defentity insert update values where select set-fields]]))

(defdb db (postgres {:db "nene"}))

(declare attestations)

(defentity attestations)

(defn get-words []
  (select attestations))

(defn attested-in? [words kana]
  (not (empty? (filter #(= (:kana %) kana) words))))

(defn attestation-for [words kana]
  (-> (filter #(= (:kana %) kana) words)
      (first)
      (get :type)
      (or "unattested")))

(defn save-word [kana type]
  (if (not (attested-in? (select attestations) kana))
    (insert attestations
            (values {:kana kana :type type}))
    (update attestations
            (set-fields {:type type})
            (where {:kana kana})))
  )