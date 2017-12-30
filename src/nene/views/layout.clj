(ns nene.views.layout
  (:require [hiccup.page :refer [html5 include-css]]))

(defn common [& body]
  (html5
    [:head
     [:title "音々〜ぎたいごぎおんご探険器具・nene, a japanese onomatopoeia explorer"]
     (include-css "/css/screen.css")]
    [:body body]))
