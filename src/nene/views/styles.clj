(ns nene.views.styles
  (:require [nene.views.styles.banner :as banner]
            [garden.stylesheet]))

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
                                :width     "12.5%"
                                :display   "inline-block"
                                :font-size "12px"}]

   [:.card {:border           "1px solid #555"
            :border-radius    "2px"
            :padding          "10px"
            :margin           "5px"
            :background-color "white"}]
   [:.word-card {:font-size "16px"}]
   [:.pull-right {:float "right"}]
   [:.modal-container {
                       :position "fixed"
                       :top      "0"
                       :left     "0"
                       :height   "100vh"
                       :width    "100vw"
                       }]
   [:.modal-background {
                        :height           "100%"
                        :width            "100%"
                        :background-color "black"
                        :opacity          "0.4"
                        :position         "absolute"
                        }]
   [:.modal {
             :position           "relative"
             :top                "80px"
             :animation-duration "0.7s"
             :animation-name     "slideup"
             :margin             "auto"
             :background-color   "white"
             :width              "1200px"
             :max-width          "80%"
             :border-radius      "5px"
             :box-shadow         "5px 5px 15px -1px #424242;"
             :padding            "15px"
             }
    [:header {:font-weight "bold" :font-size "14px"}]]
   [(garden.stylesheet/at-keyframes "slideup"
                                    [:from {:top "100vh"}]
                                    [:to {:top "80px"}])]
   [:.icon-button {:cursor "pointer" :border "none"}
    [:&:focus {:outline "none"}]
    [:&:hover {:text-decoration "underline"}]]

   (map (fn [[a c]] [(symbol (str "." a)) {:background-color c}]) attestation-color-map)
   [:.dictionary-word {:color lumber}]
   banner/styles
   [:.selected {:background-color "gold" :width "100%" :height "50vh"}]
   [:.search-box {:position "absolute"
                  :top      "40px"
                  :right    "40px"
                  :width    "300px"}
    ]
   ]
  )
