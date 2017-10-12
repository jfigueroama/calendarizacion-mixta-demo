(ns horario.edicion.utils
  (:require [cognitect.transit :as transit]
            [ajax.core :refer [GET POST]]
            [clojure.string :as string]))

(defn log
  [& data]
  (try
    (apply (partial (.-log js/console)) data)
    (catch js/Error e (.log js/console data))))

(defn info
  [& data]
  (try
    (apply (partial (.-info js/console)) data)
    (catch js/Error e (.log js/console data))))

(defn read-transit
  [tdata]
  (let [r (transit/reader :json)]
    (transit/read r tdata)))


(defn get-transit
  [url hfn]
  (GET url
       {:handler (fn load-transit-handler
                   [r]
                   (let [rt (read-transit r)]
                     (hfn rt)))
        :error-handler (fn load-transit-ehandler
                         [status status-text]
                         (.log js/console
                               (str "something bad happened: "
                                    status " " status-text)))
        :response-format :raw}))

(defn bcaller
  "Beautifull caller:
  Llama a una subscripcion o a un event handler con una syntaxis decente.
  [db [nombre_subs p1 p2 ... pN]]
  =>
  [db p1 p2 ... pN]

  ((bcaller (fn [& args] args)) {} :hola)
  "
  [hfn]
  (fn bcaller-handler [a b]
    (let [f (if (coll? a) a '(a))
          s (if (coll? b) (rest b) '())]
      (apply hfn (concat f s)))))

(defn khoras
  []
  [:7 :8 :9 :10 :11 :12 :13 :14 :15 :16 :17 :18 :19 :20])

(defn kdias
  []
  [:l :m :x :j :v])


(defn tabla-horario-vacia
  []
  (let [horas [:7 :8 :9 :10 :11 :12 :13 :14 :15 :16 :17 :18 :19 :20]
        dias  {:l nil :m nil :x nil :j nil :v nil}]
    (-> (zipmap horas (repeat (count horas) dias))
        (assoc :colisiones 0 :vacia true :multiceldas false))))

(defn hora2k
  "Convierte una hora numerica a keyworkd."
  [hora]
  (keyword (str hora)))

(defn dia2k
  "Convierte un dia numerico a keyworkd."
  [dian]
  (get {0 :l 1 :m 2 :x 3 :j 4 :v 5 :s 6 :d} dian))

(defn hora2texto
  [hora]
  (get {:7 "7:00 - 8:00"
        :8 "8:00 - 9:00"
        :9 "9:00 - 10:00"
        :10 "10:00 - 11:00"
        :11 "11:00 - 12:00"
        :12 "12:00 - 13:00"
        :13 "13:00 - 14:00"
        :14 "14:00 - 15:00"
        :15 "15:00 - 16:00"
        :16 "16:00 - 17:00"
        :17 "17:00 - 18:00"
        :18 "18:00 - 19:00"
        :19 "19:00 - 20:00"
        :20 "20:00 - 21:00" } hora))




(defn celda-data
  "Obtiene los datos completos de una celda para poder calcular colisiones.
  La bcelda es {:haid X :hcid Y :hora H ... }"
  [bcelda mhas mhcs]
  (if (vector? bcelda)
    (map #(celda-data % mhas mhcs) bcelda)
    {:ha (get-in mhas [(:haid bcelda) :new])
     :hc (get-in mhcs [(:hcid bcelda) :new])
     :hora (:hora bcelda)}))


(defn cambiar-hora
  "Cambia un horarioclase de hora de inicio en 1 o -1 hora.
   Devuelve un nuevo hc o el mismo en caso de que se salga de los rangos de
   7am a 20 horas de clase"
  [hc cambio]
  (let [hmin 7  ; hora de entrada minima
        hmax 21 ; hora de salida maxima
        nhora (+ (:hora hc) cambio)
        nhmax (+ nhora (:duracion hc))]
    (if (and (<= hmin nhora) (>= hmax nhmax))
      (assoc hc :hora nhora)
      hc)))

(defn cambiar-duracion
  "Cambia un horarioclase de duracion.
   Devuelve un nuevo hc o el mismo en caso de que se salga de los rangos de
   7am a 20 horas de clase"
  [hc cambio]
  (let [hmax 21 ; hora de salida maxima
        ndura   (+ (:duracion hc) cambio)
        hora    (:hora hc)
        horamax (+ hora ndura)]
    (if (and (<= 1 ndura) (>= hmax horamax))
      (assoc hc :duracion ndura)
      hc)))

(defn cambiar-dia
  "Cambia un horarioclase de dia. Los dias son del 0 al 4 (lunes-viernes).
   Devuelve un nuevo hc o el mismo en caso de que se salga de los rangos de
   dia 0-4."
  [hc cambio]
  (let [dmin 0  ; lunes
        dmax 4 ; viernes
        ndia (+ (:dia hc) cambio)]
    (if (and (<= dmin ndia) (>= dmax ndia))
      (assoc hc :dia ndia)
      hc)))


(defn grado
  [ngrado]
  (:nombre (get [{:id 1 :nombre "Lic."}
                 {:id 2 :nombre "Mtr."}
                 {:id 3 :nombre "Dr."}] (inc ngrado))))

(defn horarioasignacion?
  [ha]
  (and (not= (:asignacion_id ha :nada) :nada)
       (not= (:horario_id ha :nada) :nada)))
       ;(not= (:espaciot_id ha :nada) :nada)
       ;(not= (:espaciop_id ha :nada) :nada)


(defn horarioclase?
  [hc]
  (and (not= (:horarioasignacion_id hc :nada) :nada)
       (not= (:hora hc :nada) :nada)))

(defn clase2id
  "Convierte una clase (:profesor) a un \"profesor_id\"."
  [clase]
  (keyword (str (name clase) "_id")))

(defn elem-clases
  [knombre nclases]
  (let [clases
        (string/join
          "."
          (map name (filter some? nclases)))]
    (keyword
      (str (name knombre)
           (if (not (empty? clases)) ".") clases))))

(defn visible
  [tof]
  (if tof "visible" "oculto"))

(defn get-hid
  []
  (.getAttribute (.getElementById js/document "app") "data-id"))

(defn php-base-url
  []
  (.getAttribute (.getElementById js/document "app") "data-php-base-url"))
