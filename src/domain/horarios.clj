(ns domain.horarios
  (:require [domain.core :refer :all]
            [clojure.string :as st]
            [hugsql.core :as hugsql]))


;(hugsql/def-db-fns "domain/sql/horarios.sql")

(defn horario-dias
  "Define los dias en los que hay clases.
  TODO: WARNING: Esto es hardcodeado."
  []
  (vector 0 1 2 3 4))

(defn ihorarioclases
  "Devuelve los datos para un horario."
  [dbc idh]
  (q dbc "SELECT * FROM ihorarioclase WHERE horario_id=:idh" {:idh idh}))

;; ---------- NUEVAS

(defn ihorarioasignaciones
  "Eso ."
  [dbc idh]
  (q dbc
   "SELECT
        ha.id, ha.asignacion_id, ha.horario_id, ha.espaciot_id,
        ha.espaciop_id,
        a.anio, a.periodo, a.profesor_id, a.grupo_id, a.asignatura_id,
        a.multiasignacion_id,
        a.comentario, a.alumnos AS alumnos, a.thorario, asi.especialidad_id
      FROM asignacion AS a JOIN horarioasignacion AS ha
        ON ha.asignacion_id=a.id
        JOIN asignatura AS asi ON a.asignatura_id=asi.id
     WHERE horario_id=:idh" {:idh idh}))

(defn asignaciones-sin-horario
  "Asignaciones que no tienen ningun horarioasignacion para el horario actual."
  [dbc idh]
  (let [ho ($ dbc "horario" idh)
        periodos  (q dbc "SELECT * FROM multihorariomiembro WHERE multihorario_id=:midh"
                            {:midh (:multihorario_id ho)})
        rawsql (st/join " OR "
                        (map #(str "anio=" (:anio %) " AND " "periodo=\"" (:periodo %) "\"")
                             periodos))
        con-horario (map :asignacion_id
                         (q dbc "SELECT asignacion_id FROM horarioasignacion WHERE horario_id=:idh"
                            {:idh idh}))
        consul  "SELECT * FROM asignacion WHERE
         id NOT IN (:v*:ids) AND :sql:rsql"]

    ;nil))
    (q dbc consul {:ids con-horario :rsql rawsql})))

(defn hhorarioclases
  [dbc ids]
  (q dbc
     "SELECT hc.id, hc.horarioasignacion_id, hc.tipo, hc.dia, hc.hora,
             hc.duracion, hc.espacioa_id, a.multiasignacion_id
       FROM horarioclase AS hc JOIN horarioasignacion AS ha
        ON hc.horarioasignacion_id=ha.id JOIN asignacion AS a
        ON ha.asignacion_id=a.id
       WHERE hc.horarioasignacion_id IN (:v*:ids)"
     {:ids ids}))

(defn tabla
  [dbc tabl]
  (q dbc "SELECT * FROM :i:tabla" {:tabla tabl}))

(defn relacionar-mm
  "Retorna un vector con relaciones basandose en una relacion muchos a muchos."
  [dbc {:keys [tablarel tablaext atributorel atributoext]} tupla]
  (let [tr (name tablarel)
        te (name tablaext)
        ar (name atributorel)
        ae (name atributoext)
        id (:id tupla)]
    (mapv :id (q dbc
                 "SELECT :i:ae AS id FROM :i:tr WHERE :i:ar = :eid"
                 {:tr tr :ae ae :ar ar :eid id}))))

(defn relacionar-mme
  "Devuelve un vector con las relaciones encontradas muchos a muchos pero
  incluyendo atributos extra de la tabla relacional."
  [dbc {:keys [tablarel tablaext atributorel atributoext extras] } tupla]
  (let [tr (name tablarel)
        te (name tablaext)
        ar (name atributorel)
        ae (name atributoext)
        sextras (map name extras)
        id (:id tupla)]
    (mapv :id
          (q dbc
             "SELECT :i*:extras, :i:ae AS id FROM :i:tr WHERE :i:ar = :eid"
             {:tr tr :ae ae :ar ar :eid id, :extras sextras}))))

(defn hgrupos
  [dbc ids]
  ($ dbc :grupo ids))

(defn hasignaturas
  [dbc ids]
  (q dbc
     "SELECT a.id, a.codigo, a.nombre, a.carrera_id, a.especialidad_id,
      e.nombre AS especialidad
     FROM asignatura AS a LEFT JOIN especialidad AS e ON a.especialidad_id=e.id
     WHERE a.id IN (:v*:ids)"
     {:ids ids}))

(defn hprofesores
  [dbc ids]
  (q dbc
     "SELECT id, nombres, apellidos, grado FROM profesor WHERE id IN (:v*:ids)"
     {:ids ids}))

(defn hespacios
  [dbc]
  (q dbc "SELECT id, capacidad, nombre, comentario, tespacio_id, lat, lng FROM espacio"))

(defn htespacios
  [dbc]
  (q dbc "SELECT t.id, t.nombre, t.comentario, t.isvg,
         count(e.id) AS espacios
         FROM tespacio AS t JOIN espacio AS e ON t.id=e.tespacio_id
         GROUP BY t.id"))

(defn hrecursamientos
  [dbc grupos]
  (q dbc
     "SELECT r.id, r.grupo_id, r.asignacion_id, r.alumnos,
             a.grupo_id AS grupo_ext_id
        FROM recursamiento AS r JOIN asignacion AS a
              ON r.asignacion_id=a.id JOIN grupo AS g ON a.grupo_id=g.id
          WHERE r.grupo_id IN (:v*:ids)"
     {:ids (mapv :id grupos)}))

(defn hmultiasignaciones
  [dbc hasignaciones]
  (q dbc
     "SELECT id, nombre, comentario FROM multiasignacion WHERE id IN (:v*:ids)"
     {:ids (mapv :multiasignacion_id hasignaciones)}))


(defn horarios-data
  [dbc idh]
  "Retorna todos los datos necesarios para pintar un horario.

  Problema: ids repetidos. No puedo acarrear esos ids sobre Datascript. Puedo hacer
  puros lookups, pero no se si funcione.

  "
  (let [horarioasignaciones (ihorarioasignaciones dbc idh)
        ;toshow #(assoc % :_tabla false :_colisiones 0)
        toshow identity
        deco #(map toshow %)

        hpids (map :profesor_id horarioasignaciones)
        hps   (map #(assoc %
                           :cargos
                           (relacionar-mm
                                    dbc
                                    {:tablarel :profesorcargo :tablaext :cargo
                                     :atributorel :profesor_id :atributoext
                                     :cargo_id} %))
                   (deco (hprofesores dbc hpids)))

        hgids (map :grupo_id horarioasignaciones)
        hgs   (deco (hgrupos dbc hgids))

        ; integrando recursamientos
        
        hasids (map :asignatura_id horarioasignaciones)
        hass   (hasignaturas dbc hasids)

        hesp   (map #(assoc %
                            :recursos
                            (relacionar-mme
                              dbc
                              {:tablarel :espaciorecurso :tablaext :recurso
                               :atributorel :espacio_id :atributoext :recurso_id
                               :extras [:cantidad]} %))
                    (deco (hespacios dbc)))
        ; integrando recursos a cada espacio

        haids  (map :id horarioasignaciones)
        hcs   (hhorarioclases dbc haids)

        carrs (tabla dbc "carrera")
        asinh (asignaciones-sin-horario dbc idh)

        recurso (tabla dbc "recurso")
        cargo (tabla dbc "cargo")  
        especialidad (tabla dbc "especialidad")
        recursamiento (hrecursamientos dbc hgs)
        multiasignacion (hmultiasignaciones dbc horarioasignaciones)
        tespacio (htespacios dbc)
        ]


    {:horarioasignacion
     (zipmap haids horarioasignaciones)
     :profesor
     (zipmap  (map :id hps) hps)
     :grupo
     (zipmap (map :id hgs) hgs)
     :asignatura
     (zipmap (map :id hass) hass)
     :espacio
     (zipmap (map :id hesp) hesp)
     :horarioclase
     (zipmap (map :id hcs) hcs)
     :carrera
     (zipmap (map :id carrs) carrs)
     :asignaciones-sin-horario
     (zipmap (map :id asinh) asinh)
     :recurso
     (zipmap (map :id recurso) recurso)
     :cargo
     (zipmap (map :id cargo) cargo)
     :especialidad
     (zipmap (map :id especialidad) especialidad)
     :recursamiento
     (zipmap (map :id recursamiento) recursamiento)
     :multiasignacion
     (zipmap (map :id multiasignacion) multiasignacion)
     :tespacio
     (zipmap (map :id tespacio) tespacio)
     }))



;;;;;;
; Para guardar cambios


(defn horarioclase?
  "Dice si un hashmap es un horarioclase"
  [hm]
  (and (not= :nada (:hora hm :nada))
       (not= :nada (:horarioasignacion_id hm :nada))))

(defn horarioasignacion?
  "Dice si un hm es un horarioasignacion."
  [hm]
  (and (not= :nada (:horario_id hm :nada))
       (not= :nada (:asignacion_id hm :nada))))

(defn crear-hc
  [dbc hc]
  (do
    (let [gk (insert dbc
                     "INSERT INTO horarioclase (horarioasignacion_id, tipo, dia, hora, duracion, espacioa_id)
                        VALUES (:horarioasignacion_id, :tipo, :dia, :hora, :duracion, :espacioa_id)", hc)
          id (:generated_key gk)]
      (assoc hc :id id))))

(defn crear-ha
  [dbc ha]
  (do
    (let [gk (insert dbc
                     "INSERT INTO horarioasignacion (horario_id, asignacion_id, espaciot_id, espaciop_id)
                        VALUES (:horario_id, :asignacion_id, :espaciot_id, :espaciop_id)", ha)
          id (:generated_key gk)]
      (assoc ha :id id))))

(defn borrar-hc
  [dbc hc]
  (if (:_borrada hc)
    (let [brd (exec dbc "DELETE FROM horarioclase WHERE id=:id" hc)]
      (assoc hc :_borrada (if (> brd 0) true false)))
    hc))

(defn borrar-ha
  [dbc ha]
  (if (:_borrada ha)
    (let [brd (exec dbc "DELETE FROM horarioasignacion WHERE id=:id" ha)]
      (assoc ha :_borrada (if (> brd 0) true false)))
    ha))





(defn guardar-hc
  [dbc hc]
  (do
    (exec dbc
          "UPDATE horarioclase set
          hora=:hora, duracion=:duracion, dia=:dia, tipo=:tipo,
          espacioa_id=:espacioa_id 
          WHERE id=:id"
          hc)
    hc))

(defn guardar-ha
  [dbc ha]
  (do
    (exec dbc
          "UPDATE horarioasignacion set
          espaciot_id=:espaciot_id, espaciop_id=:espaciop_id
          WHERE id=:id"
          ha)
    ha))


(defn guardar-entidad
  "Guarda una entidad dependiendo de lo que venga.
  TODO agregar nuevas hc's y nuevas ha's (tiene sentido?)
  DEMO."
  [dbc commc  et]
  (if (nil? (:id et))
    (cond ; Nuevas
          (horarioasignacion? et)
          (do
            (swap! (:haid commc) inc)
            (assoc et :id @(:haid commc)))

          (horarioclase? et)
          (do
            (swap! (:hcid commc) inc)
            (assoc et :id @(:hcid commc)))

          true
          (do
            (println "Error al crear " et)
            nil))
    (if (:_borrada et)
      (cond ; borrar
        (or (horarioasignacion? et)
            (horarioclase? et))
        et

        true
        (do
          (println "Error al guardar " et)
          nil))
      (cond ; Cambios
        (or (horarioasignacion? et)
            (horarioclase? et))
        et

        true
        (do
          (println "Error al guardar " et)
          nil)))))


#_(defn guardar-entidad
  "Guarda una entidad dependiendo de lo que venga.
  TODO agregar nuevas hc's y nuevas ha's (tiene sentido?)
  "
  [dbc et]
  (if (nil? (:id et))
    (cond ; Nuevas
          (horarioasignacion? et)
          (crear-ha dbc et)

          (horarioclase? et)
          (crear-hc dbc et)

          true
          (do
            (println "Error al crear " et)
            nil))
    (if (:_borrada et)
      (cond ; borrar
            (horarioasignacion? et)
            (borrar-ha dbc et)

            (horarioclase? et)
            (borrar-hc dbc et)

            true
            (do
              (println "Error al guardar " et)
              nil))
      (cond ; Cambios
            (horarioasignacion? et)
            (guardar-ha dbc et)

            (horarioclase? et)
            (guardar-hc dbc et)

            true
            (do
              (println "Error al guardar " et)
              nil)))))


;;;;;;;;;;;;;;;
; U T I L S

(defn horario-grupos
  "Tipo: 1 lic, 2 maestria, 3 doctorado."
  [dbc idh tipo]
  (let [haids
        (map :asignacion_id
             (q dbc "SELECT asignacion_id FROM horarioasignacion
                      WHERE horario_id=:idh"
                {:idh idh}))
        gids
        (map :grupo_id
             (q dbc
                "SELECT DISTINCT a.grupo_id FROM asignacion AS a
                  JOIN grupo AS g ON a.grupo_id=g.id
                  JOIN carrera AS c ON c.id=g.carrera_id
                  WHERE a.id IN (:v*:ids) AND c.tipo=:tipo"
                {:ids haids :tipo tipo}))
        
        ]
        gids))

(defn horas-posibles-biblioteca
  []
  (vector 8 9 10 11 12 13 16 17 18))


(defn grupo-plantilla-biblioteca
  "TIpo esta hardcodeado"
  [dbc gid tipo-hc]
  (first
    (q dbc "SELECT hc.horarioasignacion_id, :tipo AS tipo, hc.hora, hc.dia,
                   hc.duracion, NULL AS espacioa_id FROM horarioclase AS hc JOIN horarioasignacion AS ha
              ON hc.horarioasignacion_id=ha.id
              JOIN asignacion AS a ON ha.asignacion_id=a.id
              WHERE a.grupo_id=:gid LIMIT 1" {:gid gid :tipo tipo-hc})))

(defn agregar-biblioteca
  [dbc idh]
  (if-let [h ($ dbc :horario idh)]
    (let [grupos (horario-grupos dbc idh)]
      nil)))


;; ---------------
;; G E N E R A C I O N  D E  H O R A R I O S
