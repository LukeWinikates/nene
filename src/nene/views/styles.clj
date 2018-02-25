(ns nene.views.styles
  (:require [garden.color :as color]))

(def gold "#B4833C")
(def tropical-blue-green "#037064")
(def medium-aquamarine "#71D5AA")
(def steel-teal "#627C8C")
(def blue-sapphire "#015C81")
(def jelly-bean-orange "#DD614A")
(def salmon "#F48668")
(def lumber "#FFE5D4")
(def charcoal "#36494E")
(def pastel-gray "#CAD2C5")

(def attestation-color-map
  {
   "dictionary-word"   blue-sapphire
   "internet-examples" gold
   "unattested"        pastel-gray
   "hard-to-pronounce" jelly-bean-orange
   "unlikely"          salmon
   "impossible"        "#FFF"
   }
  )

(def styles
  [[:.hovers {:cursor     "pointer"
              :transition ".3s border-color"
              :border     "1px solid transparent"}
    [:&:hover {:border-color steel-teal}
     ]
    ]
   [:.word-square {:background-color pastel-gray
                   :color            charcoal
                   :height           "100%"
                   :width            "100%"
                   :display          "inline-block"}
    ]
   [:.layout
    [:header {:margin "5px" :font-size "16px"}]
    [:.left-header {:text-align "right"}]
    [:.center-header {:text-align "center"}]
    [:.right-header {:text-align "center"}]
    ]

   (map (fn [[a c]] [(symbol (str "." a)) {:background-color c}]) attestation-color-map)
   [:.dictionary-word {:color lumber}]
   ]
  )
