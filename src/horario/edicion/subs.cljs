(ns horario.edicion.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [reg-sub subscribe register-sub dispatch]]
            [horario.edicion.utils :as utils]
            [horario.edicion.horarios :as h]))


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
  (sort-by (comp :apellidos :nombres) (vals (:profesor db))))
(reg-sub :profesores profesores-sub)

(defn profesor-sub
  [mprofesor [_ id]]
  (get mprofesor id))
(reg-sub :profesor :<- [:mprofesor] profesor-sub)

; ESPACIOS

(defn mespacio [db _] (get db :espacio))
(reg-sub :mespacio mespacio)

(defn espacios-sub
  [db _]
  (sort-by :nombre (vals (:espacio db))))
(reg-sub :espacios espacios-sub)

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
  "Retorna horarioclases x horarioasignacion."
  [hcs [_ ha clase id]]
  (if (not= clase :espacio)
    (filter #(= (:horarioasignacion_id %) (:id ha)) hcs)
    (let [tipo1 (if (= id (:espaciot_id ha))
                  1; Tipo teoria
                  2); Tipo Practica

          tipo2 3 ; Tipo t/p
          haid (:id ha)]

      (filter #(and (= (:horarioasignacion_id %) haid)
                    (or (= (:tipo %) tipo1)
                        (= (:tipo %) tipo2))) hcs))))
(reg-sub :hcsxha :<- [:hcs] hcsxha)


(defn hasyhcs
  "Retorna un vector de una ha con sus hcss."
  [[has hcs] [_ clase id]]
  (map (fn hasyhcs-hm [ha]
         {:ha ha
          :hcs (hcsxha hcs [nil ha clase id])})
       has))
(reg-sub
  :hasyhcs
  (fn hasyhcs-sig-sub
    [[_ clase id] _]
    [(subscribe [:hasx clase id])
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




;;;;; TABLA HORARIOS

;(defn tabla-horario-sub
;  "
;  TODO: checar colisiones"
;  [hasyhcs [_ clase id]]
;  ;(utils/info "hasyhcs" hasyhcs)
;  ;(println "hasyhcs" hasyhcs)
;  (horario/W))
;  ;[hasyhcs clase id])

;(reg-sub
;  :tabla-horario
;  (fn th-signals-sub
;    [[_ clase id] _]
;    [(subscribe [:hasyhcs clase id])])
;  tabla-horario-sub)




;;;;;;;;;;;;;;;;;;;;

; TESTING
; En la f1, el query viene en el p1, no en el 2 como en la f2.
(reg-sub
  :t
  (fn [p1 p2]
    (do
      (println "Testing dynamic subscriptions:")
      (println "f1: " "p1: " p1 ", p2: " p2)
      (reaction {:a [1 2 3] :b [4 5 6]})))
  (fn [p1 p2]
    (do
      (println "f2: " "p1: " p1 "p2: " p2))))

;;;; UI

(defn seleccionados-sub
  [hcs _]
  (filter #(true? (:_selected %)) hcs))
(reg-sub :seleccionados :<- [:hcs] seleccionados-sub)

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
