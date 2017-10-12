(ns server.components.communicator
  "Componente para el servidor que espera los mensajes del cliente, guarda los
  cambios y replica dichos cambios a todos los clientes."

  (:require [com.stuartsierra.component :as component]
            [clj-time.local :as l] 
            [clj-time.format :as f]


            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit      :refer (get-sch-adapter)]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]
            [domain.horarios :as h]))

(defn user-id-gen
  [id]
  (str "horario-" (or id 0)))

(defn communicator-handler
  "Maneja los mensajes llegados de la siguiente manera:
   Guarda las instancias en la base de datos y las remanda por el socket a
   todos los clientes con la send-fn."
  [dbc commc send-fn {:keys [event client-id uid]}]
  (let [[ev-id data] event]
    (cond
      (= :evento/cambios ev-id)
      (let [paquete data  ; alias
            usuario (or (:usuario paquete) "Anonimo")
            cambios (:cambios paquete)
            enviado_el (f/unparse (f/formatter :rfc822) (l/local-now))
            ndata (into [] (map (partial h/guardar-entidad
                                         dbc
                                         commc)
                                cambios))
            p1 (println usuario "(" enviado_el ")")
            p2 (println "Datos viejos de " uid ": "
                        data "\nDatos nuevos" ndata)
            npaquete (assoc paquete
                            :usuario usuario
                            :enviado_el enviado_el
                            :cambios cambios
                            :nuevos ndata)]

        (send-fn uid [:evento/cambios npaquete])))))



(defrecord Communicator [db
                         flag
                         router
                         rap ;ring-ajax-post
                         rag ;ring-ajax-get-or-ws-handshake
                         ch-chsk
                         chsk-send!
                         connected-uuids
                         hcid ; DEMO
                         haid ; DEMO
                         ]

 component/Lifecycle

 (start [component]
   (println ";; Starting Communicator")

   (let [user-id-fn (fn user-id-fn
                      [req]
                      (user-id-gen (get-in req [:params :hid]) ))
         {:keys [ch-recv send-fn connected-uids
                 ajax-post-fn ajax-get-or-ws-handshake-fn]}
         (sente/make-channel-socket! (get-sch-adapter)
                                     {:user-id-fn user-id-fn})
         component    (assoc component
                       :haid (atom 30000) ; id for new 
                       :hcid (atom 20000) ; id for new horarioclases
                       :flag (atom true)
                       :rap ajax-post-fn
                       :rag ajax-get-or-ws-handshake-fn
                       :ch-chsk  ch-recv               ; ChannelSocket's receive channel
                       :chsk-send! send-fn             ; ChannelSocket's send API fn
                       :connected-uids connected-uids)] ; Watchable, read-only atom

     (assoc component
            :flag (reset! (:flag component) false)
            :router (sente/start-server-chsk-router!
                      ch-recv
                      (partial communicator-handler
                               (:db component)
                               component
                               send-fn)))))

 (stop [component]
   (println ";; Stopping Communicator")
   (if-let [stop-f router]
     (assoc component :router (stop-f))
     component)))


(defn new-communicator
  []
  (map->Communicator {}))

