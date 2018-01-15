(ns nene.routes.gigo
  (:require [compojure.core :refer :all]
            [nene.analyze :as a]
            [ring.util.response :refer [response]]
            ))

(defroutes gigo-routes
           (GET "/api/gigo" [] (response [{:jp "基本" :en "Basics" :words a/words}]))
           (GET "/api/gigo/:word/relatives" [word] (response (a/relatives (a/find-word-by-romaji word))))
           (GET "/api/words" [] (response (a/variants)))
           )

