(ns trihorario.edicion.events
   (:require-macros
     [cljs.core.async.macros :as asyncm :refer (go go-loop)])
   (:require [re-frame.core :as re-frame :refer [dispatch dispatch-sync
                                                 reg-event-db reg-event-fx]]
             ; [reagent.core :as reagent]
            ; [day8.re-frame.http-fx]
             [trihorario.edicion.db :as db]
             [trihorario.edicion.utils :as utils]
             [trihorario.edicion.horarios :as h]
             [taoensso.sente :as sente]))

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


(defn msg-handler
  "Maneja los mensajes entrantes del servidor."
  [[evid paquete]]
  (cond
    (= :evento/cambios evid)
    (do
      (go
        (re-frame/dispatch [:guardar-cambios-recibidos paquete])
        (re-frame/dispatch [:integrar-cambios (:nuevos paquete)])))))

(defn socket-handler
  "Espera datos del socket y dispara eventos de actualizacion."
  [msg]
  (msg-handler (:?data msg)))


(defn crear-socket
  "Crea un websocket para comunicarse con el server y enviar/recibir cambios."
  [db hid]
  (let [socket
        (sente/make-channel-socket-client!
          (str "/socket-horarios?hid=" hid) {:type :auto})
        {:keys [chsk ch-recv send-fn state]} socket]
    (println "Socket creado para horario " hid)
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
  (fn crear-socket-evt
    [db [_ hid]]
    (crear-socket db hid)))


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


(defn cambio-para-pg
  [db entidad clase cambio]
  (let [pgenlace (keyword (str (name entidad) "_id"))
        eid (:id cambio)]
    (vector (if (= clase :horarioasignacion)
              (pgenlace cambio))
              (get-in db [:horarioasignacion (:horarioasignacion_id cambio) :new pgenlace]))))

(defn cambio-para-espacio
  [db clase cambio]
  (case clase
    :horarioasignacion
    (let [nha cambio
          oha (get-in db [:horarioasignacion (:id nha) :old])]
      (vector (:espaciot_id oha)
              (:espaciop_id oha)
              (:espaciot_id nha)
              (:espaciop_id nha)))
    :horarioclase
    (let [nhc cambio
          oha (get-in db [:horarioasignacion (:horarioasignacion_id nhc) :old])
          ohc (get-in db [:horarioclase (:id nhc) :old])]
      (vector (:espaciot_id oha)
              (:espaciop_id oha)
              (:espacioa_id ohc)
              (:espacioa_id nhc)))))

(defn eventos-nuevas-colisiones-cambio
  [db entidad cm]
  (let [clase (cond (utils/horarioasignacion? cm)
                    :horarioasignacion
                    (utils/horarioclase? cm)
                    :horarioclase)
        cmfn (case entidad
               :profesor (partial cambio-para-pg db entidad clase)
               :grupo (partial cambio-para-pg db entidad clase)
               :espacio (partial cambio-para-espacio db clase))
        eids (cmfn cm)
        ceids (filterv some? eids)]
    ceids))

(defn eventos-nuevas-colisiones
  [db cambios]
  (let [entidad (get-in db [:ui :entidad])
        neventos (mapcat (partial eventos-nuevas-colisiones-cambio db entidad)
                         cambios)]
    neventos))

(defn integrar-cambios-fx-hn
  [cofx [_ data]]
  (let [db (:db cofx)
        entidad (get-in db [:ui :entidad])
        teventos (eventos-nuevas-colisiones db data)
        ids (vec (set teventos))]
    {:db (integrar-cambios-hn db [nil data])
     :dispatch [:calcular-colisiones-entidad-paso
                entidad ids ids {}]}))

(re-frame/reg-event-fx
  :integrar-cambios
  integrar-cambios-fx-hn)


(defn enviar-cambios-hn
  ""
  [cofx _]
  (let [db (:db cofx)
        redfn (comp
                (filter #(not= (:new %) (:old %)))
                (map :new))
        nhas (into [] redfn (vals (:horarioasignacion db)))
        nhcs (into [] redfn (vals (:horarioclase db)))
        cambios (vec (concat
                       nhas
                       nhcs
                       (map #(assoc %1 :dia %2)
                            (:new-hcs db)
                            (cycle (h/filtros-dh-dias)))
                       (map #(dissoc % :id) (:new-has db))))
        paquete {:cambios cambios :usuario (get-in db [:ui :usuario])}
        send-fn (get-in db [:socket :chsk-send!])]

    (if (not (empty? cambios))
      (do
        (send-fn [:evento/cambios paquete])
        {:dispatch [:deseleccionar]})
      {})))

(re-frame/reg-event-fx
  :enviar-cambios
  enviar-cambios-hn)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Events

;; setup:
;; - inicializar db
;; - crear socket
;; - cargar informacion
;;   - calcular colisiones globales
;;
;; Aparte:
;; - cargar informacion
;;   - calcular colisiones globales
;; 
;(re-frame/dispatch-sync [:inicializar-db
;                         entidad hid usuario ekey])
;(re-frame/dispatch-sync [:cargar-datos hid])
;(re-frame/dispatch-sync [:crear-socket hid])))


(defn setup-hn
  [{:keys [entidad hid ekey mode usuario hnombre php-base-url data-url]}]
  (do
    (dispatch [:inicializar-db entidad hid hnombre usuario ekey
               php-base-url data-url])
    (dispatch [:crear-socket hid])
    (dispatch [:cargar-datos entidad hid data-url])
    {}))


(re-frame/reg-event-fx
  :setup
  (fn setup-evt
    [cofx [_ data]]
    (setup-hn data))) 


(re-frame/reg-event-db
 :inicializar-db
 (fn  inicializar-db-h
   [_ [_ entidad hid hnombre usuario ekey php-base-url data-url]]
   (println "Base de datos interna inicializada.")
   (update db/db
           :ui
           (partial
             merge {:entidad entidad
                    :hid hid
                    :hnombre hnombre
                    :usuario usuario
                    :php-base-url php-base-url
                    :data-url data-url
                    :ekey ekey}))))


(defn guardar-datos
  [db datos]
  (let [ndb (merge db datos)]
    (-> ndb
        (assoc :horarioasignacion
               (zipmap (keys (:horarioasignacion ndb))
                       (map #(hash-map :old % :new %)
                            (vals (:horarioasignacion ndb)))))
        (assoc :horarioclase
               (zipmap (keys (:horarioclase ndb))
                       (map #(hash-map :old % :new %)
                            (vals (:horarioclase ndb))))))))
(re-frame/reg-event-db
 :guardar-datos
 (fn guardar-datos-evt
   [db [_ datos]]
   (guardar-datos db datos)))

(defn cargar-datos
  [db entidad hid data-url]
  (utils/get-transit
    ;(str "/ws/horarios/data/" hid)
    data-url  ; DEMO
    (fn cargar-datos-handler
      [datos]
      (do
        (println "Datos cargados para horario " hid)
        (dispatch-sync [:guardar-datos datos])
        (dispatch-sync [:calcular-colisiones-entidad entidad]))))
  {})

(re-frame/reg-event-fx
  :cargar-datos
  (fn cargar-datos-evt
    [cofx [_ entidad hid data-url]]
    (cargar-datos (:db cofx) entidad hid data-url)))


(re-frame/reg-event-db
  :toggle
  (fn toggle-hn
    [db [_ clase id cosa]]
    (update-in db [clase id cosa] not)))

(re-frame/reg-event-db
  :toggle-fdrawer
  (fn toggle-fdrawer-hn
    [db _]
    (update-in db [:ui :fdrawer] not )))

(re-frame/reg-event-db
  :toggle-cambios-drawer
  (fn toggle-cambios-drawer-hn
    [db _]
    (update-in db [:ui :cambios-drawer] not )))


(re-frame/reg-event-db
  :toggle-dh
  (fn toggle-dh-hn
    [db [_ dh]]
    (update-in db [:filtros :dh :new dh] not)))

(re-frame/reg-event-db
  :toggle-dia
  (fn toggle-dh-hn
    [db [_ dia]]
    (let [filtros-dh-new (get-in db [:filtros :dh :new])
          dhs (map #(h/diahora2k dia %) (h/filtros-dh-horas))
          odhs (map #(get filtros-dh-new %) dhs)
          ndhs (zipmap dhs (map not odhs))]
      (update-in db [:filtros :dh :new] #(merge % ndhs )))))


(re-frame/reg-event-db
  :toggle-hora
  (fn toggle-dh-hn
    [db [_ hora]]
    (let [filtros-dh-new (get-in db [:filtros :dh :new])
          dhs (map #(h/diahora2k % hora) (h/filtros-dh-dias))
          odhs (map #(get filtros-dh-new %) dhs)
          ndhs (zipmap dhs (map not odhs))]
      (update-in db [:filtros :dh :new] #(merge % ndhs)))))

(re-frame/reg-event-db
  :toggle-tespacio
  (fn toggle-tespacio-hn
    [db [_ tid ocultar]]
    (assoc-in db [:tespacio tid :_hide] ocultar)))


(re-frame/reg-event-db
  :cambiar-espacio
  (fn cambiar-espacio-hn
    [db [_ id extid valor]]
    (let [nv (if (not= valor 0) valor)]
      (assoc-in db [:horarioasignacion id :new extid] nv))))


(re-frame/reg-event-db
  :seleccionar
  (fn seleccionar-hn
    [db [_ hcid]]
    (let [selected (get-in db [:horarioclase hcid :_selected])]
      (assoc-in db
                [:horarioclase hcid :_selected]
                (if (true? selected) false true)))))



;;;;;;;;;; UI

(re-frame/reg-event-db
  :tab
  (fn tab-hn
    [db [_ tab]]
    (assoc-in db [:ui :tab] tab)))

;;;;;;;;;;; COLISIONES

(defn calcular-colisiones-de
  ""
  [cofx [_ clase id]]
  (let [db (assoc (:db cofx) :colisiones {})
        hasyhcs (h/hasyhcs-para db clase id)
        colisiones (h/colisiones-de hasyhcs clase id)]
    {:db (assoc-in db [:colisiones clase id] colisiones )}))

(re-frame/reg-event-fx
  :calcular-colisiones
  calcular-colisiones-de)

; TODO agregar evento para recargar colisiones y estado de libre

(re-frame/reg-event-fx
  :calcular-colisiones-entidad-paso
  (fn calcular-colisiones-entidad-paso
    [{db :db} [_ entidad oids ids hcolisiones hhocupadas]]
    (if (empty? ids)
      {:db (-> db
               (assoc-in [:colisiones entidad]
                         (merge (get-in db [:colisiones entidad])
                                hcolisiones))
               (assoc-in [:ocupadas entidad]
                         (merge (get-in db [:ocupadas entidad])
                                hhocupadas))
               (assoc-in [:filtros :filtrando-dh] false))
       :dispatch [:calcular-entidades-libres-paso entidad oids {}]}
      (let [id (first ids)
            hasyhcs (h/hasyhcs-para db entidad (first ids))]
        {:dispatch [:calcular-colisiones-entidad-paso
                    entidad
                    oids
                    (rest ids)
                    (assoc hcolisiones
                           id
                           (h/colisiones-de hasyhcs entidad id))
                    (assoc hhocupadas
                           id (h/dh-ocupadas hasyhcs entidad id))
                    ] }))))
(re-frame/reg-event-fx
  :calcular-colisiones-entidad
  (fn calcular-colisiones-entidad-hn
    [cofx [_ entidad]]
    (let [db (:db cofx)
          ids (keys (get db entidad))]
      {:db (-> db
               (assoc-in [:colisiones entidad] {})
               (assoc-in [:filtros :filtrando-dh] true))
       :dispatch [:calcular-colisiones-entidad-paso
                  entidad ids ids {} {}]})))


(re-frame/reg-event-fx
  :calcular-entidades-libres-paso
  (fn calcular-entidades-libres-paso-hn
    [{db :db} [_ entidad ids hlibres]]
    (if (empty? ids)
      {:db (-> db
               (assoc-in [:libres entidad]
                         (merge (get-in db [:libres entidad])
                                hlibres))
               (assoc-in [:filtros :filtrando-dh] false))}
      (let [id (first ids)
            hasyhcs (h/hasyhcs-para db entidad (first ids))]
        {:dispatch [:calcular-entidades-libres-paso
                    entidad
                    (rest ids)
                    (assoc hlibres
                           id
                           (h/libre?
                             (get-in db [:ocupadas entidad id])
                             (h/filtros-dh-activos
                               (:filtros db))))
                    ] }))))


(re-frame/reg-event-fx
  :aplicar-filtros-dh
  (fn aplicar-filtros-dh
    [cofx _]
    (let [db (:db cofx)
          entidad (get-in db [:ui :entidad])
          ids (keys (get db entidad))]
      {:db (-> db
               (assoc-in [:filtros :dh :old]
                         (get-in db [:filtros :dh :new]))
               (assoc-in [:filtros :filtrando-dh] true))
       :dispatch [:calcular-entidades-libres-paso
                  entidad ids {}]})))


(re-frame/reg-event-db
  :abrir-espacioa
  (fn abrir-espacioa [db [_ hc_id espacioa_id]]
    (assoc-in db[:ui :dialogo-espacioa]
              {:open true :espacioa_id espacioa_id :hc_id hc_id})))

(re-frame/reg-event-db
  :cancelar-espacioa
  (fn cancelar-espacioa-hn [db]
    (assoc-in db
              [:ui :dialogo-espacioa]
              {:open false :espacioa_id nil :hc_id nil})))

(re-frame/reg-event-db
  :guardar-espacioa
  (fn guardar-espacioa-hn [db]
    (let [data (get-in db [:ui :dialogo-espacioa])
          eaid (if (zero? (:espacioa_id data)) nil (:espacioa_id data))]
      (-> db
          (assoc-in [:ui :dialogo-espacioa]
                    {:open false :espacioa_id nil :hc_id nil})
          (assoc-in [:horarioclase (:hc_id data) :new :espacioa_id]
                    eaid)))))


(re-frame/reg-event-db
  :cambiar-espacioa
  (fn cambiar-espacioa-hn
    [db [_ id]]
    (assoc-in db [:ui :dialogo-espacioa :espacioa_id] id )))


(re-frame/reg-event-fx
  :recargar-colisiones-y-libres
  (fn recargar-colisiones-y-libres-hn
    [cofx [_ entidad id]]
    {:dispatch [:calcular-colisiones-entidad-paso
                entidad [id] [id] {}]}))

(reg-event-fx
  :guardar-cambios-recibidos
  (fn guardar-cambios-recibidos-ev
    [{db :db} [_ paquete]]
    {:db (update db
                 :cambios-recibidos
                 #(conj % (-> paquete
                              (assoc :cantidad
                                     (count (:nuevos paquete)))
                              (dissoc :nuevos)
                              (dissoc :cambios))))
     :dispatch [:incrementar-cambios-recibidos (count (:nuevos paquete))]}))

(reg-event-db
  :activar-cambio-recibido
  (fn activar-cambio-recibido
    [db _]
    (update-in db [:ui :cambio-recibido] inc)))

(reg-event-db
  :desactivar-cambio-recibido
  (fn desactivar-cambio-recibido
    [db _]
    (update-in db [:ui :cambio-recibido] dec)))

(reg-event-fx
  :incrementar-cambios-recibidos
  (fn incrementar-cambios-recibidos-hn
    [{db :db} [_ cuantos]]
    {:db (update-in db
                    [:ui :cambios-recibidos-cantidad]
                    #(+ % cuantos))
     :dispatch [:activar-cambio-recibido]
     :dispatch-later [{:ms 2000 :dispatch [:desactivar-cambio-recibido]}]}))

(reg-event-fx
  :guardar-transit
  (fn guardar-transit-hn
    [{db :db} _]
    (let [hid (get-in db [:ui :hid])
          ekey (get-in db [:ui :ekey])
          ppurl (get-in db [:ui :php-base-url])
          durl (str ppurl "/horario/" hid "/feed/" ekey)]
      (do
      (utils/get-data
        durl
        #(.log js/console
               "Datos estáticos guardados en el horario."
               (js/Date.)))
      {}))))
