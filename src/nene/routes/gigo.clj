(ns nene.routes.gigo
  (:require [compojure.core :refer :all]
            [nene.models.mora :as mora]
            [ring.util.response :refer [response]]
            ))

(defroutes gigo-routes
           (GET "/api/gigo" [] (response {:category {:jp "基本" :en "Basics"} :words mora/words})))

