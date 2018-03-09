(ns nene.views.styles
  (:require [garden.color :as color]
            [nene.views.styles.banner :as banner]))

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

(def base-styles
  [
   [:body {
           :color       "#555"
           :font-family "'Hiragino Kaku Gothic Pro', 'Helvetica Neue', Helvetica, Arial, sans-serif"
           :font-size   "13px"
           :margin      "0"
           }]

   [:* {
        :box-sizing "border-box"
        }]

   [:.container {
                 :max-width "980px"
                 :margin    "auto"
                 }
    ]
   ]
  )

(def styles
  [
   base-styles
   [:.hovers {:cursor     "pointer"
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
   [:.gojuon-thumbnail-word-row {:height    "5px"
                                 :width     "12%"
                                 :display   "inline-block"
                                 :font-size "8px"}]
   [:.thumbnail-dan-column {:display "inline-block"
                            :width   "12.5%"}]
   [:.select-grouping-word-row {:height    "25px"
                                :width     "12%"
                                :display   "inline-block"
                                :font-size "12px"}]

   [:.card {:border        "1px solid #555"
            :border-radius "2px"
            :padding       "10px"
            :margin        "5px"}]
   [:.word-card {:font-size "16px"}]
   [:.pull-right {:float "right"}]
   [:.layout-viewport-left {:left "0"}]
   [:.layout-viewport-center {:left "-20vw"}]
   [:.layout-viewport-right {:left "-30vw"}]
   [:.layout
    {:display    "flex"
     :width      "120vw"
     :transition ".3s left"
     :position   "relative"}
    [:.layout-element {:display    "inline-block"
                       :height     (str "calc( 100vh - " banner/banner-height " )")
                       :overflow-y "scroll"}]
    [:.layout-left {:width "40vw"}]
    [:.layout-center {:width "60vw"}]
    [:.layout-right {:width "20vw"}]
    [:header {:margin "5px" :font-size "16px"}]
    [:.left-header {:text-align "right"}]
    [:.center-header {:text-align "center"}]
    [:.right-header {:text-align "center"}]
    ]

   (map (fn [[a c]] [(symbol (str "." a)) {:background-color c}]) attestation-color-map)
   [:.dictionary-word {:color lumber}]
   banner/styles
   [:.selected {:background-color "gold"}]
   ]
  )
