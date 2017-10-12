(ns trihorario.edicion.horarios
  "

  Manejo de las horarioclases que tienen espacios alternos:
  - Tipo 1, 2 o 3 son para la teoría, práctica o las 2 en los espacios
    designados por la horarioasignacion.
  - Tipo 4 es para la biblioteca
  - Tipo 5 es para el inglés.

    Ahora, cómo se cambian?
    Para cambiar el tipo se usa el icono de la barra de herramientas.
    Para cambiar el espacioa_id de la horarioclase se usa un dbl-click
    que va a habrir un diálogo modal para seleccionar el espacio alterno.
    Para quitarlo es lo mismo.
    Si el espacio alterno está definido entonces el tipo debe ser 1, 2 o 3.
    Si se cambia de tipo a 4 o 5 el espacio alterno será removido.

    Las colisiones de los espacios alternos se manejan igual. Las colisiones
    con la biblioteca deben detectarse también. Las colisiones con inglés
    deben mostrarse de un color distinto y no deben generar colisiones con las
    las clases normales.


  "
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [reg-sub subscribe register-sub dispatch]]
            [trihorario.edicion.utils :as utils]))

(defn multicelda?
  [celda]
  (:multicelda celda false))

(defn hay-colisiones
  "Recibe un vector de celdas simples (celda compuesta) y dice se tiene
  colisiones:
  tipo :grupo
    Si la especialidad del grupo es diferente entre si y es diferente a
    nulo.
  tipo :espacio
    Si la multiasignacion de las celdas no es la misma, hay colisiones.
  tipo :profesor
    Si los espacios son diferentes, hay colisiones.
    Si la multiasignacion es diferente o es nula, hay colisiones.
  Eso es valido para grupos"
  [tipo id celdas]
  (let [has (map :ha celdas)]
   (cond
     (= tipo :grupo)
     (let [especialidades (map :especialidad_id has)]
       (if (and (= (count (set especialidades)) (count has))
                (every? some? especialidades))
         false
         true))

     (= tipo :espacio)
     (let [mas (map :multiasignacion_id has)]
       (if (and (= (count (set mas)) 1)
                (every? some? mas))
         false
         true))

     (= tipo :profesor)
     (let [mas (map :multiasignacion_id has)]
       (if (and (= (count (set mas)) 1)
                (every? some? mas))
         false
         true)))))

(defn th-integrar
  "Integra un hc a la tabla de horario."
  [clase id ta ha hc hora]
  (let [thora  (utils/hora2k hora)
        tdia   (utils/dia2k (:dia hc))
        celda  (-> ta thora tdia)
        hh    {:haid (:id ha) :ha ha :hcid (:id hc) :hc hc :hora hora :multicelda false}
        ncelda (cond
                 (nil? celda)
                 hh
                 (multicelda? celda)
                 (update celda :celdas #(conj % hh))
                 (not (multicelda? celda))
                 {:multicelda true :celdas [celda hh] :colisiones false})
        nccelda (if (and (multicelda? ncelda)
                         (not (:colisiones ncelda)))
                  (assoc ncelda :colisiones
                                (or (:colisiones ncelda)
                                    (hay-colisiones clase id (:celdas ncelda))))
                  ncelda)]

    (-> ta
        (assoc-in [thora tdia] nccelda))))
        ; TODO quitar multiceldas y vacia

(def tipo-recurso
  {:individual  1
   :grupal      2
   :grupo       3
   })

(def tipo-horarioclase
  {:teoria          1
   :practica        2
   :teoriapractica  3
   :biblioteca      4
   :ingles          5
   :asesorias       6})

(def hc-tipo
  {1 :teoria
   2 :practica
   3 :teoriapractica
   4 :biblioteca
   5 :ingles
   6 :asesorias})


(defn thg-hc
  "Toma una tabla grupo, un ha y varios hc. Los devuelve integrados en la
  tabla."
  [clase id ta ha hc]
  (loop [t ta horas 0]
    (if (< horas (:duracion hc))
      (let [tipoc (:tipo hc)]
        (if (or
              (nil? (#{(:biblioteca tipo-horarioclase)
                       (:ingles tipo-horarioclase)
                       (:asesorias tipo-horarioclase)} tipoc))
              (and (or (= tipoc (:biblioteca tipo-horarioclase))
                       (= tipoc (:ingles tipo-horarioclase)))
                   (= clase :grupo))
              (and (= tipoc (:asesorias tipo-horarioclase))
                   (= clase :profesor)))
          (recur (th-integrar clase id t ha hc (+ (:hora hc) horas)) (+ horas 1))
          (recur t (+ horas 1))))
      t)))

(defn thg-hcs
  "Toma una tabla de grupo, una ha y varios hc. Los devuelve integrados en la
  tabla."
  [clase id ta ha hcs]
  (loop [t ta hcs hcs]
    (if (empty? hcs)
      t
      (recur (thg-hc clase id t ha (first hcs)) (rest hcs)))))

;(defn tabla-horario-grupo
;  "Lista de [ha hcs], osea, horarioasignacion con horarioclases"
;  [hasyhcs [_ gid]]
;  (let [tabla (utils/tabla-horario)]
;    (loop [t tabla hhs hasyhcs]
;      (if (empty? hhs)
;        t
;        (let [hh  (first hhs)
;              ha  (:ha hh)
;              hcs (:hcs hh)]
;          (recur (thg-hcs t ha hcs) (rest hhs)))))))


(defn tabla-horario
  "Lista de [ha hcs], osea, horarioasignacion con horarioclases"
  [hasyhcs clase id]
  (let [tabla (utils/tabla-horario-vacia)]
    (loop [t tabla hhs hasyhcs]
      (if (empty? hhs)
        t
        (let [hh  (first hhs)
              ha  (:ha hh)
              hcs (:hcs hh)]
          (recur (thg-hcs clase id t ha hcs) (rest hhs)))))))


(defn expandir-horas
  [hc]
  (let [hora (:hora hc)
        dur  (:duracion hc)
        dia  (:dia hc)
        duras (range 0 dur)]

    (map #(keyword (str "d" dia "h" (+ hora %)))
         duras)))

(defn expandir-hc
  [hc]
  (map (fn ehcm [h] (update hc :hora #(+ % h)))
       (range 0 (:duracion hc))))


(defn colision-profesor
  "Colisiones en un dia/hora para un profesor."
  [hcs]
  (let [gmas (reduce conj
                         #{}
                         (map :multiasignacion_id hcs))
        gma (first gmas)
        cuantas (count gmas)]
    (if (and (= 1 cuantas)
             (some? gma))
      0
      (dec (count hcs)))))

(defn colision-espacio
  "Colisiones en un dia/hora para un espacio."
  [hcs]
  (let [gmas (reduce conj
                         #{}
                         (map :multiasignacion_id hcs))
        gma (first gmas)
        cuantas (count gmas)]
    (if (and (= 1 cuantas)
             (some? gma))
      0
      (dec (count hcs)))))

(defn colision-grupo
  "Colisiones en un dia/hora para un grupo.
  Si solo hay 1 multiasignacion y no es nula
  o  hay tantas especialidades como clases y ningna es nula.
  "
  [hcs]
  (let [cuantas (count hcs)
        gmas (reduce conj #{} (map :multiasignacion_id hcs))
        gma (first gmas)
        mcuantas (count gmas)
        esps (reduce conj #{} (map :especialidad_id hcs))
        esp (first esps)
        ecuantas (count esps)
        ]
    (if (or (and (= 1 mcuantas)
                 (some? gma))
            (and (= ecuantas (count hcs))
                 (= 0 (count (filter #(nil? (:especialidad_id %))
                                     hcs)))))
      0
      (dec (count hcs)))))




(defn colisiones-profesor
  "Colisiones de un profesor (cantidad):

  "
  [hcs]
  (let [horas (group-by (fn [hc] (str "d" (:dia hc) "h" (:hora hc)))
                        hcs)
        posibles (filter #(> (count %) 1) (vals horas))]
    (reduce + (map colision-profesor posibles))))
 
(defn colisiones-espacio
  "Colisiones de un espacio (cantidad):

  "
  [hcs]
  (let [horas (group-by (fn [hc] (str "d" (:dia hc) "h" (:hora hc)))
                        hcs)
        posibles (filter #(> (count %) 1) (vals horas))]
    (reduce + (map colision-espacio posibles))))


(defn colisiones-grupo
  [hcs]
  (let [horas (group-by (fn [hc] (str "d" (:dia hc) "h" (:hora hc)))
                        hcs)
        posibles (filter #(> (count %) 1) (vals horas))]
    (reduce + (map colision-grupo posibles))))

(defn hcsfromhas
  [hasyhcs]
  (flatten (map :hcs hasyhcs)))


(defn hasyhcs-para
  [db clase id]
  (let [has (map :new (vals (:horarioasignacion db)))
        hcs (map :new (vals (:horarioclase db)))
        enlace (keyword (str (name clase) "_id"))
        ]
    (cond
      (#{:profesor :grupo} clase)
      (let [chas (filter #(= (enlace %) id) has)]
        (mapv (fn [ha]
                {:ha ha
                 :hcs 
                 (map
                   #(assoc %
                           :especialidad_id
                           (:especialidad_id ha))
                   (filter #(= (:id ha) 
                               (:horarioasignacion_id %))
                           hcs))}) chas))
      (= :espacio clase)
      (let [ahaids (set (map :horarioasignacion_id
                             (filter #(= (:espacioa_id %) id) hcs)))
            nhas (filter #(or (= (:espaciot_id %) id)
                              (= (:espaciop_id %) id)
                              (ahaids (:id %))) has)]
        (mapv (fn hasyhcs-ha-fn [ha]
                {:ha ha
                 :hcs (filter
                        (fn hasyhcs-hc-fn [hc]
                          (let [tipo (:tipo hc)]
                            (and (= (:id ha)
                                    (:horarioasignacion_id hc))
                                 (or (= (:espacioa_id hc) id)
                                     (and (nil? (:espacioa_id hc))
                                          (or (= (tipo-horarioclase :teoriapractica)
                                                 tipo)
                                              (and (= (:espaciot_id ha) id)
                                                   (= (tipo-horarioclase :teoria) tipo))
                                              (and (= (:espaciop_id ha) id)
                                                   (= (tipo-horarioclase :practica)
                                                      tipo))))))))
                        hcs)}) nhas)))))



(defn hcsfromhas-expandidos
  [hasyhcs]
  (mapcat expandir-hc (hcsfromhas hasyhcs)))

(defn colisiones
  "Calcula las colisiones de un conjunto de hasyhcs para una clase e id.
  TODO: El calculo se hace con la union de las intersecciones del primer
  ha (hcs) con los demas, el segundo ha (hcs) con los demas, etc.

  TODO: Rehacer el calculo de colisiones de las tablas para reaprovechar
  las reglas actuales. Los ha's pueden no tener que colisionar si tiene
  clases al mismo tiempo, si se cumplen sus reglas basicas de dominio."

  [hasyhcs clase id]
  (case clase
    :profesor (colisiones-profesor (hcsfromhas-expandidos hasyhcs))
    :grupo (colisiones-grupo (hcsfromhas-expandidos hasyhcs))
    :espacio (colisiones-espacio (hcsfromhas-expandidos hasyhcs))))


(defn colisiones-de
  [hasyhcs clase id]
  (colisiones hasyhcs clase id))



;(defn colisiones-para
;  [[hasyhcs] [_ clase id]]
;  (colisiones hasyhcs clase id))

(defn colisiones-de-sub
  [colisiones [_ clase id]]
  (get-in colisiones [clase id]))


(defn colisiones-clase
  "Colisiones globales para una clase"
  [colisiones [_ clase]]
  (let [datos (filter some? (vals (get colisiones clase)))]
    (if-not (empty? datos)
      (reduce + datos))))

(defn nombre-dia
  [ndia]
  ({0 "Lunes"
    1 "Martes"
    2 "Miércoles"
    3 "Jueves"
    4 "Viernes"
    5 "Sábado"
    6 "Domingo"} ndia))


;;;;;;; FILTROS DIA / HORA


;(hc-ocupada-fn
;  "Crea una funcion que dice si hc es libre en ciertos dias/horas (con or como
;   base entre diferentes dias"
;  [sdias shoras]
;  (fn hc-libre-fni [hc]
;    (let [horas (set (range (:hora hc) (+ (:hora hc) (:duracion hc))))]
;      (if (sdias (:dia hc))
;        (if (not-empty (clojure.set/intersection horas

(defn filtros-dh-dias
  []
  [0 1 2 3 4])

(defn filtros-dh-horas
  []
  [7 8 9 10 11 12 13 14 15 16 17 18])

(defn diahora2k
  [d h]
  (keyword (str "d" d "h" h)))


(defn filtros-dh-activos
  [filtros]
  (set
    (map (fn [[k v]] k)
         (filter (fn [[k v]] (true? v))
                 (get-in filtros [:dh :old])))))


(defn libre?
  "Dice si esta libre una entidad segun sus las horas ocupadas que se tienen
  y libres que se necesitan."
  [ocupadas libres]
  (if (empty? libres)
    true
    (let [hlibres (clojure.set/difference libres ocupadas)]
      (not (not= hlibres libres)))))
      

(defn hcs-libres-dh
  "Retorna los hcs que esta libres en los dias/horas dados.
  "
  [db dias horas]
  (let [sdias (set dias)
        shoras (set horas)
        hcs (map :old (vals (:horarioclase db)))
        eids (keys (:espacio db))
                
        ]
    []))

(defn dh-ocupadas
  [hasyhcs entidad id]
  (if (= entidad :espacio)
    (let [phcs (hcsfromhas-expandidos hasyhcs)
          filtro (fn [hc]
                   (#{:teoria :practica :teoriapractica}
                              (get hc-tipo (:tipo hc))))
          hcs (filter filtro phcs)
          dhs (map #(diahora2k (:dia %) (:hora %)) hcs)]
      (set dhs))
    []))
