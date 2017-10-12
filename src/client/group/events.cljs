(ns client.group.events
    (:require [re-frame.core :as re-frame]
             ; [reagent.core :as reagent]
              [day8.re-frame.http-fx]
              [client.group.db :as db]
              [client.utils :as utils]))

;; Interceptors
(def myinters
  [;check-spec-interceptor               ;; ensure the spec is still valid
;   (when ^boolean js/goog.DEBUG utils/log)  ;; look in your browser console 4 logs
   ;trim-v
   ])


;; Events

(re-frame/reg-event-db
 :inicializar-db
 (fn  [_ _]
   db/db))

(re-frame/reg-event-db
 :guardar-datos
 (fn  [db [_ datos]]
   (let [ndb (merge db datos)]
     (-> ndb
         (assoc :horarioasignacion
                (zipmap (keys (:horarioasignacion ndb))
                        (map #(hash-map :old % :new %)
                             (vals (:horarioasignacion ndb)))))
         (assoc :horarioclase
                (zipmap (keys (:horarioclase ndb))
                        (map #(hash-map :old % :new %)
                             (vals (:horarioclase ndb))))) ))))


(re-frame/reg-event-fx
  :cargar-datos
  myinters
  (fn [cofx [_ hid]]
   (do
     (utils/get-transit (str "/ws/horarios/data/" hid)
                        (fn cargar-datos-handler
                          [datos]
                          (re-frame/dispatch [:guardar-datos datos])))
     {} )))


(re-frame/reg-event-fx
  :alertar
  myinters
  (fn [cofx [_ alerta]]
    (do
      (.alert js/window alerta)
      {})))


(re-frame/reg-event-fx
  :mostrar-tabla-grupo
  (fn mostrar-tabla-hn
    [cofx [_ grupo]]
    (let [db (:db cofx)]
      {:db (-> db
               (assoc-in [:grupo grupo :_tabla] true)) })))
       

(re-frame/reg-event-fx
  :ocultar-tabla-grupo
  (fn ocultar-tabla-hn
    [cofx [_ grupo]]
    (let [db (:db cofx)]
      {:db (-> db
               (assoc-in [:grupo grupo :_tabla] false)) })))
 
(re-frame/reg-event-fx
  :mostrar-data-grupo
  (fn mostrar-data-hn
    [cofx [_ grupo]]
    (let [db (:db cofx)]
      {:db (-> db (assoc-in [:grupo grupo :_data] true)) })))
       

(re-frame/reg-event-fx
  :ocultar-data-grupo
  (fn ocultar-data-hn
    [cofx [_ grupo]]
    (let [db (:db cofx)]
      {:db (-> db (assoc-in [:grupo grupo :_data] false)) })))
      
(re-frame/reg-event-db
  :cambiar
  (fn reset-entity-handler
    [db [_ tipo id attr valor]]
    (assoc-in db [tipo id attr] valor)))

(re-frame/reg-event-db
  :cambiar2
  (fn reset-entity-handler
    [db [_ tipo id attr1 attr2 valor]]
    (assoc-in db [tipo id attr1 attr2 ] valor)))

(re-frame/reg-event-fx
  :focus
  (fn focus-event
    [cofx [_ sele]]
    (do
      (let [e (.querySelector js/document sele)]
        (if e
          (.focus e)))
      {})))

(re-frame/reg-event-db
  :seleccionar
  (fn seleccionar-hn
    [db [_ hcid]]
    (let [selected (get-in db [:horarioclase hcid :_selected]) ]
      (assoc-in db
                [:horarioclase hcid :_selected]
                (if (true? selected) false true)))))




;(reg-event-fx                             ;; note the trailing -fx
;  :handler-with-http                      ;; usage:  (dispatch [:handler-with-http])
;  (fn [{:keys [db]} _]                    ;; the first param will be "world"
;    {:db   (assoc db :show-twirly true)   ;; causes the twirly-waiting-dialog to show??
;     :http-xhrio {:method          :get
;                  :uri             "https://api.github.com/orgs/day8"
;                  :timeout         8000                                           ;; optional see API docs
;                  :response-format (ajax/json-response-format {:keywords? true})  ;; optional see API docs
;                  :on-success      [:good-http-result]
;;                  :on-failure      [:bad-http-result]}}))



