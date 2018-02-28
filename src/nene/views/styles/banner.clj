(ns nene.views.styles.banner)

(def banner-height "110px")

(def styles
  [:.banner
   {
    :display               "grid"
    :grid-template-columns "100px 1fr"
    :grid-template-rows    "48px 1fr"
    :line-height           "1.3"
    :margin-bottom         "20px"
    :height                banner-height
    }
   [:h1.jp {:font-size "48px"}]
   [:h1.en {:font-size "30px"}]
   [:h1 {:margin     0
         :text-align "center"}]

   [:h3 {:margin "5px 0"}]
   [:.banner-bloc {:display "inline-block"}]
   ]
  )
