(defproject calendarizacion-mixta-demo "0.1.0-SNAPSHOT"
  :description "Sistema de Construccion de Horarios"
  :dependencies [[org.clojure/clojure "1.8.0"]

                 ; Clientes
                 ;[org.clojure/clojurescript "1.9.521"] ; compila
                 [org.clojure/clojurescript "1.9.542"] ; compila
                 ;[org.clojure/clojurescript "1.9.562"]
                 [reagent "0.6.0"
                  :exclusions
                  [org.clojure/tools.reader cljsjs/react]]

                 ;[binaryage/devtools "0.8.3"]
                 [re-frame "0.9.2" :exclusions [cljsjs/react]]
                 ;[cljsjs/react-with-addons "15.4.2-2"]
                 [cljs-ajax "0.5.8"]
                 ;[cljsjs/material-ui "0.16.4-0"]
                 [cljs-react-material-ui "0.2.30"] ; "0.2.37"
                  ;:exclusions
                  ;[org.clojure/tools.reader cljsjs/react cljsjs/react-dom]

                 [com.cognitect/transit-cljs "0.8.239"]
                 [keybind "2.0.0"]
                 [figwheel-sidecar "0.5.9"]
                 ;[quil "2.6.0"]


                 ; Server
                 ;[org.clojure/data.json "0.2.6"] ; json
                 [com.stuartsierra/component "0.3.1"]
                 [com.cognitect/transit-clj "0.8.295"]
                 [http-kit "2.2.0"]    ; web server
                 [cheshire "5.6.3"]    ; json
                 [ring/ring-core "1.5.0"] ; handlers
                 [javax.servlet/servlet-api "2.5"] ; manejo de datos POST
                 [compojure "1.5.1"]    ; web framework
                 [com.cognitect/transit-clj "0.8.288"]
                 [com.taoensso/sente "1.10.0"]
                 [hiccup "1.0.5"]

                 ; Utiles
                 ;[org.clojure/core.async "0.2.395"]
                 [org.clojure/core.async "0.3.442"]
                 [funcool/cats "2.1.0"]
                 [funcool/lentes "1.1.0"]

                 ; DB
                 [com.layerware/hugsql "0.4.7"]
                 [mysql/mysql-connector-java "6.0.4"]

                 ; Testing
                ; [org.clojure/tools.trace "0.7.9"]
                 [org.clojure/test.check "0.9.0"]

                 ; Otros
                 [hawk "0.2.10"]  ; File watcher
                 [clj-time "0.12.0"]
                 [lein-light-nrepl "0.3.3"]
                 [defun "0.3.0-RC1"]]



  :main server.core

  :plugins [[lein-cljsbuild "1.1.4"]; :exclusions [[org.clojure/clojure]]

            [lein-ancient "0.6.10"]]

  :min-lein-version "2.5.3"

  :source-paths ["src"] ;[ "src/clj"]

  :clean-targets
  ^{:protect false}  ["resources/public/js/trihorario/edicion/out"
                      "resources/public/js/trihorario/edicion/devcards_out"
                      "resources/public/js/trihorario/edicion/out/app-dev.js"
                      "resources/public/js/trihorario/edicion/out/app-devcards.js"
                      "resources/public/js/trihorario/edicion/out/app.js"
                      "target"]

  :figwheel {:css-dirs ["resources/public/css"]}
             ;:hawk-options {:watcher :polling}


  :profiles
  {:dev
   {:dependencies [ [binaryage/devtools "0.8.2"]]
                   ;[devcards "0.2.2" :exclusions [cljsjs/react cljsjs/react-dom]]]

    :plugins      [ [lein-figwheel "0.5.9"]]}}

   ;:devcards {:dependencies [[binaryage/devtools "0.8.2"]
   ;                          [devcards "0.2.2" :exclusions [cljsjs/react cljsjs/react-dom]]]
   ;           :plugins      [[lein-figwheel "0.5.9"]]}}


  :cljsbuild
  {:builds
   [{:id           "tridev" ; trihorario-edicion-dev
     :source-paths ["src/trihorario/edicion"]
     :figwheel     {:load-warninged-code false}
     :compiler     {:main                 trihorario.edicion.core
                    :output-to            "resources/public/js/trihorario/edicion/app-dev.js"
                    :output-dir           "resources/public/js/trihorario/edicion/out"
                    :asset-path           "/js/trihorario/edicion/out"
                    :closure-defines      {goog.DEBUG true}
                    ;:preloads             [devtools.preload]
                    ;:external-config      {:devtools/config
                    ;                       {:features-to-install :all}}
                    :source-map-timestamp true}}
    {:id           "triprod"
     :source-paths ["src/trihorario/edicion"]
     :compiler     {:main            trihorario.edicion.core
                    :output-to       "resources/public/js/trihorario/edicion/app.js"
                    :optimizations   :advanced
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}
    ]}


    ; Edicion x grupos
    ;{:id           "group-dev"
    ; :source-paths ["src/client/"]
    ; :figwheel     {:on-jsload "client.group.core/mount-root"
    ;                :load-warninged-code true
    ;                }
    ; :compiler     {:main                 client.group.core
    ;                :output-to            "public/js/group/app.js"
    ;                :output-dir           "public/js/group/out"
;
;                    :asset-path           "/public/js/group/out"
 ;                   :source-map-timestamp true}}
;
;    {:id           "group-prod"
;     :source-paths ["src/client"]
;     :compiler     {:main            client.group.core
;                    :output-to       "public/js/group/app-prod.js"
;                    :optimizations   :advanced
;                    :closure-defines {goog.DEBUG false}
;                    :pretty-print    false}}




  :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]})
