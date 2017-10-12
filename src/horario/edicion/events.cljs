(ns horario.edicion.events
   (:require-macros
     [cljs.core.async.macros :as asyncm :refer (go go-loop)])
   (:require [re-frame.core :as re-frame]
             ; [reagent.core :as reagent]
            ; [day8.re-frame.http-fx]
             [horario.edicion.db :as db]
             [horario.edicion.utils :as utils]
             [taoensso.sente :as sente]))

;;; TODO implementar el cambio de un hc para ser de tipo typ (3), porque
;;; hay muchas asignaciones que necesitan los 2 espacios a la vez.
;;; TODO limpiar las tablas del sistema para ser utf8. SOLO los campos
;;; que usas.


;; Interceptors
(def myinters
  []);check-spec-interceptor               ;; ensure the spec is still valid
;   (when ^boolean js/goog.DEBUG utils/log)  ;; look in your browser console 4 logs
;   re-frame/trim-v


(re-frame/reg-event-db
  :assoc-in
  (fn assoc-in-ev
    [db [_ path va]]
    (assoc-in db path va)))

(re-frame/reg-event-db
  :update-in
  (fn assoc-in-ev
    [db [_ path vfn]]
    (update-in db path vfn)))

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
      #_(utils/info "Integrando " evid data)
      (go
        (re-frame/dispatch [:integrar-cambios data])))))

(defn socket-handler
  "Espera datos del socket y dispara eventos de actualizacion."
  [msg]
  (do
    #_(utils/info "Recibido: " msg)
    (msg-handler (:?data msg))))


(defn crear-socket-hn
  "Crea un websocket para comunicarse con el server y enviar/recibir cambios."
  [db [_ hid]]
  (let [socket
        (sente/make-channel-socket-client!
          (str "/socket-horarios?hid=" hid) {:type :auto})
        {:keys [chsk ch-recv send-fn state]} socket]

    (assoc db
           :socket
           {:chsk chsk
            :ch-recv ch-recv        ; ChannelSocket's receive channel
            :chsk-send! send-fn     ; ChannelSocket's send API fn
            :chsk-state state
            :router (sente/start-client-chsk-router! ch-recv socket-handler)})))
              ; Watchable, read-only atom

(re-frame/reg-event-db
  :crear-socket
  crear-socket-hn)

(re-frame/reg-event-db
  :crear-hc
  (fn crear-hc-hn
    [db [_ nhc]]
    (update db :new-hcs #(conj % nhc))))

(re-frame/reg-event-db
  :crear-ha
  (fn crear-ha-hn
    [db [_ nha]]
    (-> db
      (update :new-has #(conj % nha))
      (assoc-in  [:asignaciones-sin-horario (:id nha) :_pendiente] true))))


(defn cambiar-hc
  [hc cual cambio]
  (cond
    (= :hora cual)
    (utils/cambiar-hora hc cambio)
    (= :duracion cual)
    (utils/cambiar-duracion hc cambio)
    (= :dia cual)
    (utils/cambiar-dia hc cambio)
    (= :tipo cual)
    (assoc hc :tipo cambio)))

(defn cambiar-hcs-db
  [db cual cambio]
  (let [selected (into []
                       (comp (filter #(true? (:_selected %)))
                             (map :new))
                       (vals (:horarioclase db)))]
    (loop [ndb db nsels selected]
      (if (empty? nsels)
        ndb
        (recur (update-in ndb
                          [:horarioclase (:id (first nsels)) :new]
                          #(cambiar-hc % cual cambio))
               (rest nsels))))))


(re-frame/reg-event-db
  :cambiar-hora
  (fn cambiar-hora-hn
    [db [_ cambio]]
    (cambiar-hcs-db db :hora cambio)))

(re-frame/reg-event-db
  :cambiar-tipo
  (fn cambiar-tipo-hn
    [db [_ tipo]]
    (cambiar-hcs-db db :tipo tipo)))




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
          asinh (:asignaciones-sin-horario db)
          renfn #(assoc % :new (:old %))
          penfn #(assoc % :_pendiente false)]
     (-> db
         (assoc :horarioasignacion
                (zipmap (keys mhas) (map renfn (vals mhas))))
         (assoc :horarioclase
                (zipmap (keys mhcs) (map renfn (vals mhcs))))
         (assoc :new-has [] :new-hcs []
                :asignaciones-sin-horario
                (zipmap (keys asinh) (map penfn (vals asinh))))))))

(re-frame/reg-event-db
  :deseleccionar
  (fn deseleccionar-hn
    [db _]
    (let [mhas (:horarioasignacion db)
          mhcs (:horarioclase db)
          renfn #(assoc % :_selected false)]
     (-> db
         (assoc :horarioasignacion
                (zipmap (keys mhas) (map renfn (vals mhas))))
         (assoc :horarioclase
                (zipmap (keys mhcs) (map renfn (vals mhcs))))))))

(defn borrar-ents
  [db ents clase]
  (loop [ndb db nents ents]
    (if (empty? nents)
      ndb
      (recur (assoc-in ndb [clase (:id (first nents)) :new] (first nents))
             (rest nents)))))

(re-frame/reg-event-db
  :borrar
  (fn borrar-hn
    [db _]
    (let [mhas (vals (:horarioasignacion db))
          smhas (map :new (filter :_selected mhas))
          mhcs (vals (:horarioclase db))
          smhcs (map :new (filter :_selected mhcs))
          renfn #(assoc % :_borrada true)
          nmhas (map renfn smhas)
          nmhcs (map renfn smhcs)]

      (-> db
          (borrar-ents nmhas :horarioasignacion)
          (borrar-ents nmhcs :horarioclase)))))

(defn integrar-cambios-hn
  "Integra los cambios de la base de datos a los datos en la interfaz.
   Recibe un vector con entidades que deben guardarse. Dichas entidades son
   horarioasignacion y horarioclase."
  [db [_ cambios]]
  (loop [ndb db cbs cambios]
    (if (empty? cbs)
      (assoc ndb :new-hcs [] :new-has []) ; limpiando new-hcs para no guardar los viejos cambios
      (let [ent (first cbs)
            clase (cond (utils/horarioasignacion? ent) :horarioasignacion
                        (utils/horarioclase? ent) :horarioclase
                        true (do (utils/info "Cambio desconocido" ent) nil))
            oent (get-in ndb [clase (:id ent)])
            nent (if (some? oent)
                   (if (not (:_borrada ent))
                     (assoc oent :old ent :new ent))  ; TODO hacer que el usuario sepa si fue cambio suyo o de otro.
                  (hash-map :old ent :new ent))]
        (recur (if (some? clase)
                 (if (some? nent)
                   (if (= clase :horarioasignacion)
                     (-> ndb
                         (assoc-in [clase (:id ent)] nent)
                         (assoc :asignaciones-sin-horario
                                (dissoc (:asignaciones-sin-horario ndb) (:asignacion_id ent))))
                     (assoc-in ndb [clase (:id ent)] nent))
                   (assoc ndb clase (dissoc (get ndb clase) (:id ent))))
                 ndb)
               (rest cbs))))))

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
        cambios (vec (concat nhas nhcs (:new-hcs db) (map #(dissoc % :id) (:new-has db))))
        send-fn (get-in db [:socket :chsk-send!])]

    (if (not (empty? cambios))
      (do
        #_(utils/info "Enviando" cambios)
        (send-fn [:evento/cambios cambios])
        {:dispatch [:deseleccionar]})
      {})))

(re-frame/reg-event-fx
  :enviar-cambios
  enviar-cambios-hn)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Events

(re-frame/reg-event-db
 :inicializar-db
 (fn  inicializar-db-h
   [_ [_ hid usuario]]
   (-> db/db
       (assoc-in [:ui :hid] hid)
       (assoc-in [:ui :usuario] usuario))))

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
                             (vals (:horarioclase ndb)))))))))


(re-frame/reg-event-fx
  :cargar-datos
  myinters
  (fn [cofx [_ hid]]
   (do
     (utils/get-transit (str "/ws/horarios/data/" hid)
                        (fn cargar-datos-handler
                          [datos]
                          (do
                            (utils/info "Datos cargados para horario " hid)
                            (re-frame/dispatch [:guardar-datos datos]))))
     {})))


(re-frame/reg-event-fx
  :alertar
  myinters
  (fn [cofx [_ alerta]]
    (do
      (.alert js/window alerta)
      {})))


(re-frame/reg-event-db
  :toggle
  (fn toggle-hn
    [db [_ clase id cosa]]
    (update-in db [clase id cosa] #(not %))))

(re-frame/reg-event-db
  :cambiar
  (fn cambiar-hn
    [db [_ tipo id attr valor]]
    (assoc-in db [tipo id attr] valor)))

(re-frame/reg-event-db
  :cambiar2
  (fn reset-entity-handler2
    [db [_ tipo id attr1 attr2 valor]]
    (assoc-in db [tipo id attr1 attr2 ] valor)))

(re-frame/reg-event-db
  :cambiar-espacio
  (fn cambiar-espacio-hn
    [db [_ id extid valor]]
    (let [nv (if (not= valor 0) valor)]
      (assoc-in db [:horarioasignacion id :new extid] nv))))



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
    (let [selected (get-in db [:horarioclase hcid :_selected])]
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


;;;;;;;;;; UI

(re-frame/reg-event-db
  :tab
  (fn tab-hn
    [db [_ tab]]
    (assoc-in db [:ui :tab] tab)))


