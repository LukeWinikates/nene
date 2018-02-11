(defproject nene "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.5.2"]
                 [hiccup "1.0.5"]
                 [ring/ring-json "0.4.0"]
                 [ring-server "0.4.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.postgresql/postgresql "42.2.0"]
                 [korma "0.4.3"]
                 [ragtime "0.7.2"]
                 [garden "1.3.3"]
                 ]
  :plugins [[lein-ring "0.8.12"]]
  :aliases {"migrate"  ["run" "-m" "nene.migrate/migrate"]
            "rollback" ["run" "-m" "nene.migrate/rollback"]}
  :ring {:handler nene.handler/app
         :init nene.handler/init
         :destroy nene.handler/destroy}
  :profiles
  {:uberjar {:aot :all}
   :production
   {:ring
    {:open-browser? false, :stacktraces? false, :auto-reload? false}}
   :dev
   {:dependencies [[ring-mock "0.1.5"] [ring/ring-devel "1.5.1"]]}})
