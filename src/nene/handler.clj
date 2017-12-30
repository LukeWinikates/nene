(ns nene.handler
  (:require [compojure.core :refer [defroutes routes]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.file-info :refer [wrap-file-info]]
            [hiccup.middleware :refer [wrap-base-url]]
            [ring.middleware.json :refer [wrap-json-response]]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [nene.routes.home :refer [home-routes]]
            [nene.routes.gigo :refer [gigo-routes]]))

(defn init []
  (println "nene is starting"))

(defn destroy []
  (println "nene is shutting down"))

(defroutes app-routes
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (-> (routes home-routes gigo-routes app-routes)
      (handler/site)
      (wrap-json-response)
      (wrap-base-url)))
