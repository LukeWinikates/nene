(ns nene.attesting
  (:require [korma.db :refer [defdb postgres]]
            [korma.core :refer [defentity insert values where select]]))

(defdb db (postgres {:db "nene"}))

(declare words)

(defentity words)

;TODO: implement reasonable caching for this
(defn get-words []
  (select words))

(defn attested-in? [words word]
  (not (empty? (filter #(= (:word %) word) words))))

(defn save-word [word]
  (if (not (attested-in? (select words) word))
    (insert words
            (values {:word word}))
    ))