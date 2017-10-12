(ns client.events
   (:require-macros
     [cljs.core.async.macros :as asyncm :refer (go go-loop)])
    (:require [re-frame.core :as re-frame]
             ; [reagent.core :as reagent]
              [day8.re-frame.http-fx]
              [client.group.db :as db]
              [client.utils :as utils]
              [cljs.core.async :as async :refer [<! >! put! chan]]
              ;[taoensso.sente :as sente :refer (cb-success?)] 
              [taoensso.sente :as sente]))

;; Interceptors
(def myinters
  [;check-spec-interceptor               ;; ensure the spec is still valid
;   (when ^boolean js/goog.DEBUG utils/log)  ;; look in your browser console 4 logs
;   re-frame/trim-v
   ])


;(re-frame/reg-event-db
; :inicializar-db
; (fn  [_ _]
;   db/db))


(defn msg-handler
  "Maneja los mensajes entrantes del servidor."
  [[evid data]]
  (cond
    (= :evento/cambios evid)
    (do
      (utils/info "Integrando" data)
      (go
        (re-frame/dispatch [:integrar-cambios data])))))

(defn socket-handler
  "Espera datos del socket y dispara eventos de actualizacion."
  [msg]
  (do
    (utils/info msg)
    (msg-handler (:?data msg))))


(defn crear-socket-hn
  "Crea un websocket para comunicarse con el server y enviar/recibir cambios."
  [db _]
  (let [socket
        (sente/make-channel-socket-client! "/socket-horarios" {:type :auto })
        {:keys [chsk ch-recv send-fn state]} socket
        ]
    (assoc db
           :socket
           {:chsk chsk
            :ch-recv ch-recv        ; ChannelSocket's receive channel
            :chsk-send! send-fn     ; ChannelSocket's send API fn
            :chsk-state state 
            :router (sente/start-client-chsk-router! ch-recv socket-handler)
            } ))) ; Watchable, read-only atom

(re-frame/reg-event-db
  :crear-socket
  crear-socket-hn)

(defn cambiar-hc
  [hc cual cambio]
  (cond
    (= :hora cual)
    (utils/cambiar-hora hc cambio)
    (= :duracion cual)
    (utils/cambiar-duracion hc cambio)
    (= :dia cual)
    (utils/cambiar-dia hc cambio)))

(defn cambiar-hcs-db
  [db cual cambio]
  (let [selected (into []
                       (comp (filter #(true? (:_selected %)))
                             (map :new) )
                       (vals (:horarioclase db))) ]
    (loop [ndb db nsels selected]
      (if (empty? nsels)
        ndb
        (recur (update-in ndb
                          [:horarioclase (:id (first nsels)) :new]
                          #(cambiar-hc % cual cambio))
               (rest nsels)) ))))


(re-frame/reg-event-db
  :cambiar-hora
  (fn cambiar-hora-hn
    [db [_ cambio]]
    (cambiar-hcs-db db :hora cambio)))

(re-frame/reg-event-db
  :cambiar-dia
  (fn cambiar-dia-hn
    [db [_ cambio]]
    (cambiar-hcs-db db :dia cambio)))

(re-frame/reg-event-db
  :cambiar-duracion
  (fn cambiar-duracion-hn
    [db [_ cambio]]
    (cambiar-hcs-db db :duracion cambio)))


(re-frame/reg-event-db
  :cancelar-cambios
  (fn cancelar-cambios-hn
    [db _]
    (let [mhas (:horarioasignacion db)
          mhcs (:horarioclase db)
          renfn #(assoc % :new (:old %)) ]
    (-> db
        (assoc :horarioasignacion
               (zipmap (keys mhas) (map renfn (vals mhas))))
        (assoc :horarioclase
               (zipmap (keys mhcs) (map renfn (vals mhcs))))))))

(re-frame/reg-event-db
  :deseleccionar
  (fn deseleccionar-hn
    [db _]
    (let [mhas (:horarioasignacion db)
          mhcs (:horarioclase db)
          renfn #(assoc % :_selected false) ]
    (-> db
        (assoc :horarioasignacion
               (zipmap (keys mhas) (map renfn (vals mhas))))
        (assoc :horarioclase
               (zipmap (keys mhcs) (map renfn (vals mhcs))))))))


(defn integrar-cambios-hn
  "Integra los cambios de la base de datos a los datos en la interfaz.
   Recibe un vector con entidades que deben guardarse. Dichas entidades son
   horarioasignacion y horarioclase."
  [db [_ cambios]]
  (loop [ndb db cbs cambios]
    (if (empty? cbs)
      ndb
      (let [ent (first cbs)
            clase (cond (utils/horarioasignacion? ent) :horarioasignacion
                        (utils/horarioclase? ent) :horarioclase
                        true (do (utils/info "Cambio desconocido" ent) nil))
            oent (get-in ndb [clase (:id ent)])
            nent (if (some? oent)
                  (assoc oent :old ent :new ent)  ; TODO hacer que el usuario sepa si fue cambio suyo o de otro.
                  (hash-map :old ent :new ent)) ]
        (recur (if (some? clase)
                 (assoc-in ndb [clase (:id ent)] nent)
                 ndb) (rest cbs)) ))))

(re-frame/reg-event-db
  :integrar-cambios
  integrar-cambios-hn)


(defn enviar-cambios-hn
  ""
  [cofx _]
  (let [db (:db cofx)
        redfn (comp
                (filter #(not= (:new %) (:old %)))
                (map :new))
        nhas (into [] redfn (vals (:horarioasignacion db)))
        nhcs (into [] redfn (vals (:horarioclase db)))
        cambios (vec (concat nhas nhcs))
        send-fn (get-in db [:socket :chsk-send!])
        ]
    (if (not (empty? cambios))
      (do
        (utils/info "Enviando" cambios)
        (send-fn [:evento/cambios cambios])
        {:dispatch [:deseleccionar]} )
      {}) ))

(re-frame/reg-event-fx
  :enviar-cambios
  enviar-cambios-hn)
