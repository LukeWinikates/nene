(ns nene.routes.gigo
  (:require [compojure.core :refer :all]
            [nene.models.mora :as mora]
            [ring.util.response :refer [response]]
            ))

(defroutes gigo-routes
           (GET "/api/gigo" [] (response [{:jp "基本" :en "Basics" :words mora/words}]))
           (GET "/api/gigo/:word/relatives" [word] (response (mora/relatives (mora/find-word-by-romaji word))))
           (GET "/api/words" [] (response (mora/variants)))
           )

