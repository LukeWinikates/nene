(ns nene.views.layout
  (:require [hiccup.page :refer [html5 include-css include-js]]
            [garden.core :refer [css]]
            [nene.views.styles :refer [styles]]))

(defn common [& body]
  (html5
    [:head
     [:title "音々〜ぎたいごぎおんご探険器具・nene, a japanese onomatopoeia explorer"]
     (include-js "/js/main.js")
     [:style (css styles)]
     ]
    [:body body]))
