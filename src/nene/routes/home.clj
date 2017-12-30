(ns nene.routes.home
  (:require [compojure.core :refer :all]
            [nene.views.layout :as layout]))

(defn home []
  (layout/common
    [:header.banner
     [:div.banner-block.title
      [:h1.en "nene"]
      [:h1.jp "音々"]]
     [:div.banner-block.teaser
      [:h3.en "Gitaigo & Giongo Explorer"]
      [:h3.jp "ぎたいご・ぎおんご探険器具"]]]))

(defroutes home-routes
           (GET "/" [] (home)))
