(ns horario.edicion.horarios
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
            [horario.edicion.utils :as utils]))

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

(def tipo-horarioclase
  {:teoria          1
   :practica        2
   :teoriapractica  3
   :biblioteca      4
   :ingles          5
   :asesorias       6})


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

(defn colisiones
  "Calcula las colisiones de un conjunto de hasyhcs para una clase e id.
  TODO: El calculo se hace con la union de las intersecciones del primer
  ha (hcs) con los demas, el segundo ha (hcs) con los demas, etc.

  TODO: Rehacer el calculo de colisiones de las tablas para reaprovechar
  las reglas actuales. Los ha's pueden no tener que colisionar si tiene
  clases al mismo tiempo, si se cumplen sus reglas basicas de dominio.
  "
  [hasyhcs clase id]
  (let [hcs (flatten (map :hcs hasyhcs))
        horas (flatten
                (map expandir-horas hcs))]

    0))

