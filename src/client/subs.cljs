(ns client.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [reg-sub subscribe register-sub dispatch]]
            [client.utils :as utils]))


(defonce db re-frame.db/app-db)



(defn asignaciones-sub
  [db _]
  (vals (:asignaciones db)))

(defn has-sub
  [db _]
   (map :new (vals (:horarioasignacion db))))

(defn hcs-sub
  [db _]
  (map :new (vals (:horarioclase db))))

(defn carreras-sub
  [db _]
  (sort-by :nombre (vals (:carrera db))))

(defn grupos-sub
  [db _]
  (vals (:grupo db)))

(defn grupo-sub
  [db [_ gid]]
  (-> db :grupo (get gid)))

(defn profesores-sub
  [db _]
  (sort-by (comp :apellidos :nombres) (vals (:profesor db))))

(defn asignaturas-sub
  [db _]
   (vals (:asignatura db)))

(defn gxcarrera
  [grupos [_ carrera]]
  (map :id (filter #(= (:carrera_id %) carrera) grupos)))

(defn hasxgrupo
  "Retorna horarioasignaciones x grupo."
  [has [_ gid]]
  (filter #(= (:grupo_id %) gid) has))

(defn hcsxha
  "Retorna horarioclases x horarioasignacion."
  [hcs [_ ha]]
  (filter #(= (:horarioasignacion_id %) (:id ha)) hcs))

(defn hasyhcs
  "Retorna un vector de una ha con sus hcss."
  [[ghas hcs] [_ gid]]
  (map (fn hasyhcs-hm [ha]
         {:ha ha
          :hcs (hcsxha hcs [nil ha])})
       ghas))


(defn mprofesor [db _] (get db :profesor))

(defn masignatura [db _] (get db :asignatura))

(defn mespacio [db _] (get db :espacio))

(defn mgrupo [db _] (get db :grupo))

(defn mhorarioclase [db _] (get db :horarioclase))

(defn mhorarioasignacion [db _] (get db :horarioasignacion))


(defn th-integrar
  "Integra un hc a la tabla de horario."
  [ta ha hc hora]
  (let [thora (utils/hora2k hora)
        tdia  (utils/dia2k (:dia hc))
        celda (-> ta thora tdia)
        hh    {:haid (:id ha) :hcid (:id hc) :hora hora}
        ncelda (cond
                 (nil? celda)
                 hh
                 (vector? celda)
                 (conj celda hh)
                 (map? celda)
                 [celda hh]) ]
    (assoc-in ta [thora tdia] ncelda)))


(defn thg-hc
  "Toma una tabla grupo, un ha y varios hc. Los devuelve integrados en la
  tabla."
  [ta ha hc]
  (loop [t ta horas 0]
    (if (< horas (:duracion hc))
      (recur (th-integrar t ha hc (+ (:hora hc) horas)) (+ horas 1))
      t)))

(defn thg-hcs
  "Toma una tabla de grupo, una ha y varios hc. Los devuelve integrados en la
  tabla."
  [ta ha hcs]
  (loop [t ta hcs hcs]
    (if (empty? hcs)
      t
      (recur (thg-hc t ha (first hcs)) (rest hcs)))))

(defn tabla-horario-grupo
  "Lista de [ha hcs], osea, horarioasignacion con horarioclases"
  [hasyhcs [_ gid]]
  (let [tabla (utils/tabla-horario)]
    (loop [t tabla hhs hasyhcs]
      (if (empty? hhs)
        t
        (let [hh  (first hhs)
              ha  (:ha hh)
              hcs (:hcs hh)]
          (recur (thg-hcs t ha hcs) (rest hhs)))))))

(defn seleccionados-sub
  [hcs _]
  (filter #(true? (:_selected %)) hcs))

