(ns nene.routes.words
  (:require [compojure.core :refer :all]
            [nene.analyze :as a]
            [ring.util.response :refer [response]]
            [nene.attesting]
            ))

(defroutes words-routes
           (GET "/api/words" [] (response (a/variants)))
           (POST "/api/attestations" request (response
                                               (nene.attesting/save-word
                                                 (get-in request [:body "kana"])
                                                 (get-in request [:body "attestation-type"]))))
           )

