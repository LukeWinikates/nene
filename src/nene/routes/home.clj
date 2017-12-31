(ns nene.routes.home
  (:require [compojure.core :refer :all]
            [nene.views.layout :as layout]))

(defn home []
  (layout/common
    [:header.banner
     [:h1.en {:style "grid-column: 1; grid-row:1; align-self:end"} "nene"]
     [:h1.jp {:style "grid-column: 1; grid-row:2; align-self:start;"} "音々"]
     [:h3.en {:style "grid-column: 2; grid-row:1; align-self:end"} "Gitaigo & Giongo Explorer"]
     [:h3.jp {:style "grid-column: 2; grid-row:2; align-self:start"} "ぎたいご・ぎおんご探険器具"]]
    ;]
    [:section.main
     [:script "Elm.Main.embed(document.querySelector(\".main\"))"]]))

(defroutes home-routes
           (GET "/" [] (home)))
