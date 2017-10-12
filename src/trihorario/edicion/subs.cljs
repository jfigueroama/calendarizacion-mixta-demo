(ns trihorario.edicion.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [reg-sub subscribe register-sub dispatch]]
            [trihorario.edicion.utils :as utils]
            [trihorario.edicion.horarios :as h]))


; Para debugg
(defonce db re-frame.db/app-db)

; GENERALES

(reg-sub :db (fn [db _] db))

(defn asignaciones-sub
  [db _]
  (vals (:asignaciones db)))
(reg-sub :asignaciones asignaciones-sub)

(defn asignaturas-sub
  [db _]
  (vals (:asignatura db)))
(reg-sub :asignaturas  asignaturas-sub)

(reg-sub
  :hcs-full
  (fn hcs-full-hn
    [db _]
    (vals (:horarioclase db))))

(reg-sub
  :has-full
  (fn has-full-hn
    [db _]
    (vals (:horarioasignacion db))))

(defn has-sub
  [db _]
  (map :new (vals (:horarioasignacion db))))
(reg-sub :has has-sub)

(defn hcs-sub
  [db _]
  (map :new (vals (:horarioclase db))))
(reg-sub :hcs hcs-sub)

(defn masignatura [db _] (get db :asignatura))
(reg-sub :masignatura masignatura)

(reg-sub
  :cargo
  (fn cargo-sub
    [db _]
    (:cargo db)))


(reg-sub
  :cargos
  (fn cargos-sub
    [db _]
    (vals (:cargo db))))

(reg-sub
  :recurso
  (fn recurso-sub
    [db _]
    (:recurso db)))

; GRUPOs

(defn carreras-sub
  [db _]
  (sort-by :nombre (vals (:carrera db))))
(reg-sub :carreras carreras-sub)

(defn grupos-sub
  [db _]
  (vals (:grupo db)))
(reg-sub :grupos grupos-sub)

(defn mgrupo [db _] (get db :grupo))
(reg-sub :mgrupo mgrupo)

(defn grupo-sub
  [mgrupo [_ id]]
  (get mgrupo id))
(reg-sub
  :grupo
  :<- [:mgrupo]
  grupo-sub)

(defn gxcarrera
  [grupos [_ carrera]]
  (filter #(= (:carrera_id %) carrera) grupos))
(reg-sub
  :gxcarrera
  :<- [:grupos]
  gxcarrera)

; PROFESORES

(defn mprofesor [db _] (get db :profesor))
(reg-sub :mprofesor mprofesor)

(defn profesores-sub
  [db _]
  (sort-by :apellidos (vals (:profesor db))))
(reg-sub :profesores profesores-sub)

(defn profesor-sub
  [mprofesor [_ id]]
  (get mprofesor id))
(reg-sub :profesor :<- [:mprofesor] profesor-sub)

; ESPACIOS

(reg-sub
  :tespacios
  (fn tespacios-sub
    [db _]
    (sort-by :nombre (vals (:tespacio db)))))

(defn mespacio [db _] (get db :espacio))
(reg-sub :mespacio mespacio)

(defn espacios-sub
  [db _]
  (sort-by :nombre (vals (:espacio db))))
(reg-sub :espacios espacios-sub)

(reg-sub
  :espaciosx
  :<- [:espacios]
  (fn espaciosx-sub
    [espacios [_ tid]]
    (filter #(= tid (:tespacio_id %)) espacios)))

(defn espacio-sub
  [mespacio [_ id]]
  (get mespacio id))
(reg-sub :espacio :<- [:mespacio] espacio-sub)

; HORARIOS

(defn mhorarioasignacion [db _] (get db :horarioasignacion))
(reg-sub :mhas mhorarioasignacion)

(reg-sub
  :ha
  :<- [:mhas]
  (fn ha-sub
    [mhas [_ id]]
    (get mhas id)))

(defn mhorarioclase [db _] (get db :horarioclase))
(reg-sub :mhcs mhorarioclase)

(reg-sub
  :hc
  :<- [:mhcs]
  (fn hc-sub [mhcs [_ id]]
    (get mhcs id)))


(defn hasx
  "Retorna horarioasignaciones x grupo."
  [has [_ clase id]]
  (if (not= clase :espacio)
    (let [kclase (utils/clase2id clase)]
      (filter #(= (kclase %) id) has))
    (let [kclaset (utils/clase2id :espaciot)
          kclasep (utils/clase2id :espaciop)]
      (filter #(or (= (kclaset %) id)
                   (= (kclasep %) id))
              has))))
(reg-sub :hasx :<- [:has] hasx)

(defn hcsxha
  "Retorna horarioclases x horarioasignacion.
  TODO: Incluir horas de biblioteca, ingles o asesorias."
  [hcs [_ ha clase id]]
  (if (not= clase :espacio)
    (map #(assoc % :especialidad_id (:especialidad_id ha))
         (filter #(= (:horarioasignacion_id %) (:id ha)) hcs))
    (let [tipo1 (if (= id (:espaciot_id ha))
                  1; Tipo teoria
                  2); Tipo Practica

          tipo2 3 ; Tipo t/p
          haid (:id ha)]

      (filter #(and (= (:horarioasignacion_id %) haid)
                    (or (and (or (= (:tipo %) tipo1)
                                 (= (:tipo %) tipo2))
                             (nil? (:espacioa_id %)))
                        (= (:espacioa_id %) id))) hcs))))
(reg-sub :hcsxha :<- [:hcs] hcsxha)


(defn hasyhcs
  "Retorna un vector de una ha con sus hcss."
  [[has hcs] [_ clase id]]
  (let [nhas (cond
               (not= :espacio clase)
               has
               (= :espacio clase)
               (let
                 [ohasx   (hasx has [nil :espacio id])
                  ahaids (clojure.set/union
                           (set
                             (map :horarioasignacion_id
                                  (filter #(= (:espacioa_id %) id)
                                          hcs)))
                           (set
                             (map :id ohasx)))]
                 (filter #(ahaids (:id %)) has)))]
    (map (fn hasyhcs-hm [ha]
           {:ha ha
            :hcs (hcsxha hcs [nil ha clase id])})
         nhas)))

(reg-sub
  :hasyhcs
  (fn hasyhcs-sig-sub
    [[_ clase id] _]
    [(subscribe [:hasx clase id])
     (subscribe [:hcs])])
  hasyhcs)

(reg-sub
  :hasyhcs-espacio
  (fn hasyhcs-espacio-sig-sub
    [[_ clase id] _]
    [(subscribe [:has])
     (subscribe [:hcs])])
  hasyhcs)

(defn espacios-usados-sub
  [[espacios has] _]
  (sort-by :nombre
           (filter
             (fn espacios-usados-sub-f1
               [e]
               (some #(or (= (:id e) (:espaciot_id %))
                          (= (:id e) (:espaciop_id %)))
                     has))
             espacios)))
(reg-sub :espacios-usados :<- [:espacios] :<- [:has] espacios-usados-sub)



(reg-sub
  :cantidad-diferentes
  :<- [:has-full]
  :<- [:hcs-full]
  (fn diferentes-sub
    [[has hcs] _]
    {:has (count (filter #(not= (:new %) (:old %)) has))
     :hcs (count (filter #(not= (:new %) (:old %)) hcs))}))

;;;;; TABLA HORARIOS

;;;; UI

(defn seleccionados-sub
  [hcs _]
  (filter :_selected hcs))
(reg-sub
  :seleccionados
  :<- [:hcs-full]
  seleccionados-sub)

(defn borrados-sub
  [hcs _]
  (filter :_borrada hcs))
(reg-sub
  :borrados
  :<- [:hcs]
  borrados-sub)

(reg-sub
 :ui
 (fn ui-sub
   [db _]
   (:ui db)))

(reg-sub
 :new-has
 (fn new-has-sub
   [db _]
   (:new-has db)))

(reg-sub
 :new-hcs
 (fn new-hcs-sub
   [db _]
   (:new-hcs db)))

(reg-sub
 :asignaciones-sin-horario
 (fn asinh-sub
   [db _]
   (vals (:asignaciones-sin-horario db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defn colisiones-sub
;  "Retorna un vector de una ha con sus hcss."
;  [[hasyhcs] [_ clase id]]
;  (utils/log "Recalculando para " clase id)
;  (h/colisiones hasyhcs clase id))

;(reg-sub
;  :colisiones
;  (fn colisiones-sig-sub
;    [[_ clase id] _]
;    [(subscribe [:hasyhcs clase id])])
;  colisiones-sub)

(reg-sub
 :colisiones
 (fn colisiones-sub
   [db _]
   (:colisiones db)))

(reg-sub
 :colisiones-de
 :<- [:colisiones]
 h/colisiones-de-sub)


(reg-sub
 :colisiones-clase
 :<- [:colisiones]
 h/colisiones-clase)



;;;;;;;;;;;;;;;
(reg-sub
  :ocupadas
  (fn ocupadas-sub
    [db _]
    (:ocupadas db)))

(reg-sub
  :ocupadas-de
  :<- [:ocupadas]
  (fn ocupadas-de-sub
    [ocupadas [_ clase id]]
    (get-in ocupadas [clase id])))


(reg-sub
  :libres
  (fn libres-sub
    [db _]
    (:libres db)))

(reg-sub
  :libres-cantidad-entidad
  :<- [:libres]
  (fn libres-cantidad-entidad-sub
    [libres [_ entidad]]
    (let [elibres (get libres entidad)]
      (count (filter (fn [[k v]] (or (true? v) (nil? v)))
                     elibres)))))


(reg-sub
  :libre-de
  :<- [:libres]
  (fn libres-de-sub
    [libres [_ clase id]]
    (get-in libres [clase id])))


;;;;;;;;;;;;;;;;;;
(reg-sub
 :fdrawer
 :<- [:ui]
 (fn fdrawer-sub
   [ui _]
   (:fdrawer ui)))

(reg-sub
  :filtros
  (fn filtros-sub [db _]
    (:filtros db)))

(reg-sub
  :filtros-dh
  :<- [:filtros]
  (fn filtros-dh-sub
    [filtros _]
    (:dh filtros)))


(reg-sub
  :filtros-dh-activos
  :<- [:filtros]
  (fn filtros-dh-activos-sub
    [filtros _]
    (h/filtros-dh-activos filtros)))

(reg-sub
 :filtrando-dh
 :<- [:filtros]
 (fn filtrando-dh-sub [ui _]
   (:filtrando-dh ui)))

(reg-sub
 :filtros-profes
 :<- [:filtros]
 (fn filtros-profes-sub
   [filtros _]
   (:profes filtros)))

(reg-sub
 :dialogo-espacioa
 :<- [:ui]
 (fn dialogo-espacioa-sub
   [ui _]
   (:dialogo-espacioa ui)))


(reg-sub
  :colisiones-tespacio
  (fn colisiones-tespacio-set-sub
    [[_ tid] _]
    [(subscribe [:espaciosx tid])
     (subscribe [:colisiones])])
  (fn colisiones-tespacio-sub
    [[espacios colisiones] [_ tid]]
    (let [eids (set (map :id espacios))
          colis (filter (fn [[k v]]
                          (eids k))
                        (:espacio colisiones))]
      (reduce + (map (fn [[k v]] v) colis)))))

(reg-sub
  :cambios-recibidos
  (fn cambios-recibidos-sub
    [db _]
    (:cambios-recibidos db)))

(reg-sub
  :cambios-recibidos-orden
  :<- [:cambios-recibidos]
  (fn cambios-recibidos-sub
    [cambios _]
    (reverse cambios)))

(reg-sub
  :cambio-recibido
  :<- [:ui]
  (fn cambio-recibido-sub
    [ui _]
    (:cambio-recibido ui)))

(reg-sub
  :cambios-recibidos-cantidad
  :<- [:ui]
  (fn cambios-recibidos-sub
    [ui _]
    (:cambios-recibidos-cantidad ui)))
