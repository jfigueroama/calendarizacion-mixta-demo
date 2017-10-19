(ns server.core
  (:require [com.stuartsierra.component :as component]
            [server.components.app  :as appns])
  (:gen-class))


(defn app
  [config]
  (component/system-using
    (appns/new-app config)
    {:handler [:db :comm]
     :webserver [:handler]
     :comm [:db]}))

(defn -main
  "TODO: Recibir la configuracion como parametros"
  []
  (let [config (or (try (read-string (slurp "config.edn")) (catch Exception e))
                   {:mode :prod
                    :db {:subname "//localhost:3306/ca?useUnicode=yes&characterEncoding=UTF-8&serverTimezone=UTC"
                         :user "root"
                         :db "ca?useUnicode=yes&characterEncoding=UTF-8&serverTimezone=UTC"
                         :password ""
                         :useUnicode "yes"
                         :characterEncoding "UTF-8"}
                    :webapp {:port 9002
                             :mode :prod
                             :base-url "http://127.0.0.1:9002"
                             :admin-site-url "http://127.0.0.1/ca/shp/?"
                             :solver-call ["python" "main.py" "RS" :dir "./solver/lluvia"] }})
        system (app config)]
    (do
      (component/start system))))



