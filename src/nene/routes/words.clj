(ns nene.routes.words
  (:require [compojure.core :refer :all]
            [nene.analyze :as a]
            [ring.util.response :refer [response]]
            [nene.attesting]
            ))

(defroutes words-routes
           (GET "/api/words" [] (response (a/variants)))
           (POST "/api/words/:word/attest" [word] (response (nene.attesting/save-word word)))
           )

