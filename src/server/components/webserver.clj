(ns server.components.webserver
  (:require [com.stuartsierra.component :as component]
            [org.httpkit.server :refer [run-server]]

            [compojure.core :refer :all]))



(defrecord WebServer [options server handler]
  component/Lifecycle
  (start [component]
    (let [handler (get-in component [:handler :handler])
          ;handler (routes
          ;          (GET "/" [] (str "root"))
          ;          (GET "/get" [] (str "es get")))
          server (run-server handler options)]
      (println ";; Starting Web Server at port " (:port options) )
      (assoc component :server server)))
  (stop [component]
    (println ";; Stoping Web Server")
    (when server
      (server)
      (assoc component :server nil))))

(defn new-webserver
  [options]
  (map->WebServer {:options options}))

;;;;

