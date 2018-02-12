(ns nene.views.styles)

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
    [:&.attested {:background-color blue-sapphire
                  :color            lumber}]]]
  )
