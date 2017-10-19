(ns server.components.app
  "Componente para la app."
  (:require [com.stuartsierra.component :as component]
            [server.components.webserver :as webserver]
            [server.components.handler :as handler]
            [server.components.database  :as database]
            [server.components.communicator :as comm]))

(defrecord App [config
                webserver webapp comm db]
 component/Lifecycle

 (start [component]
   (println ";; Starting App in mode " (:mode config))
   (let [system (component/start-system component [:db :handler :comm :webserver])]
     (println "Please open the url " (-> config :webapp :base-url))
     system ))
 (stop [component]
   (println ";; Stopping App")
   (component/stop-system component [:db :handler :comm :webserver])))

(defn new-app
  [config]
  (map->App
    {:config config
     :db (database/new-database (:db config))
     :handler (handler/new-handler (:webapp config))
     :webserver (webserver/new-webserver (:webapp config))
     :comm (comm/new-communicator)}))


(defn new-app1
  [config]
  (component/system-map
       :db (database/new-database (:db config))
       :comm (comm/new-communicator)
       :handler (handler/new-handler (:webapp config))
       :webserver (webserver/new-webserver (:webapp config))))

