(ns nene.views.styles)

(def styles
  [:.hovers {:cursor     "pointer"
             :transition ".3s border-color"
             :border "1px solid transparent"}
   [:&:hover {:border-color "darkgray"}
    ]
   ]
  )