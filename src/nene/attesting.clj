(ns nene.attesting
  (:require [korma.db :refer [defdb postgres]]
            [korma.core :refer [defentity insert values]]))

(defdb db (postgres {:db "nene"}))

(declare words)

(defentity words)

(defn save-word [word]
  (insert words
          (values {:word word}))
  )
