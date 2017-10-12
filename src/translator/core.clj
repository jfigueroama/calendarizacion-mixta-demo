;;; Traductor de restricciones semanticas del sistema de generaicon de horarios
;;;
;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TODO: Definirlo con clojure.spec
;;;
;;; chorario (configuracion de horario):
;;; {"lgs": [ lecture group 1, lecture group 2 ... lecture group N ],
;;;  "samehourtm": true
;;;  }
;;;
;;; lecture-group:
;;; {"lectures": [ lecture 1, lectur 2, ... lecture N ],
;;;  "hours": [],
;;;  "days": [] }
;;;
;;; lecture:
;;; {"t": "t",  ; o "p" (teoria o practica)
;;;  "n": 5,    ; cantidad de clases
;;;  "d": 1 }   ;  duracion de las clases
;;;
;;; restriccionh (restriccion horario):
;;; {"nodays": [0, 3],    ; Ni lunes ni juves.
;;;  "nohours": [8, 9] }  ; Ni a las 8 ni a las 9 de cualquier dia.
;;; 
;;;
;;; -------- >>>>>>
;;;
;;; metabound:
;;; {:multiassignment: N o nil
;;; :assignments [assignment-data1 assignment-data2 ... ]
;;; :gtid SID   // sacados de la asignacion o multiasignacion
;;; :gpid SID   // sacados de la asignacion o multiasignacion
;;; :samehourtp false // misma hora teoria y practica (parte parcial t, parte p)
;;; :lgs [{:lectures [ {:t :t :n 5 :d 1}  ] ; lecture groups
;;;        :index nil
;;;        :perm nil} ]
;;;        
;;; :tperm false   ; old
    ;:pperm false   ; old
    ;:gperm false   ; old
    ;:tmhi true     ; old
    ;:pmhi true     ; old
    ;:gmhi false      ; old
;;;;  theory [tdata1 tdata2 ...]    ; old, pero requerida por Lluvia
;;; :practice [pdata1 pdata2 ...]  ; old
    ;)
;;; }
(ns translator.core
  (:use [translator.utils]
        [clojure.set])
  (:require [clojure.data.json :as json]
;            [lentes.core :as l]
         
           [org.httpkit.client :as http]

;            [clojure.spec :as s]
;            [clojure.spec.gen :as gen]
;            [translator.spec.lecture :as le]
;            [translator.spec.lecture-group :as lg]
;            [translator.spec.chorario :as ch]
))


(defn $
  "Localiza un objeto por su id en una tabla de la kb.
  id puede ser una secuencia de ids."
  [kb ent id]
  (if (or (seq? id) (coll? id))
    (map (partial $ ent) id)
      (kget (kget kb ent) id)))

(defn sons
  "Localiza los hijos de id referenciados por kb en ent."
  [kb ent fk id]
  (let [sid (get-iid id)
        ents (vals (kget kb ent))]
    (filter #(= (get-iid (kget % fk)) sid)
            ents)))
(defn ents
  [kb ent]
  (vals (kget kb ent)))

(defn q
  [kb ent ofn]
  (filter ofn (vals (kget kb ent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Del dominio del problema


(defn sons-tr
  "Hijos a traves de: Localiza los hijos de from en in, pero relacionados
  a traves de una tabla relacional. No hay de otra mas que meter las
  relaciones. Estan todas las nuestras.
  
   del gespacio 13 en espacio
  (sons-tr :gespacio :espacio :60)
  "
  [kb from in id]
  (let [inters {:gespacio { :espacio {:ent :gespaciomiembro
                                      :fk  :gespacio_id
                                      :k   :espacio_id }}
                :espacio { :recurso {:ent  :espaciorecurso
                                     :fk   :espacio_id
                                     :k    :recurso_id}}
                :asignacion {:recurso {:ent :asignacionrecurso
                                      :fk :asignacion_id
                                      :k :recurso_id}}
                :recurso { :espacio {:ent :espaciorecurso
                                     :fk :recurso_id
                                     :k :espacio_id}}
                }
        mdata (get (get inters from) in)
        ent (get mdata :ent)
        fk (get mdata :fk)
        k (get mdata :k)
        ients  (if ent
                 (sons kb ent fk id)
                 '())
        iients (map #(% k) ients) ]
    (map #($ kb in %) iients)))


(defn configuration
  [kb k]
  "Devuelve el valor de la configuracion ke o nil"
  (let [data (get kb :scheduling)]
    (kget data k)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCESO a los datos en la kb
(defn assignments
  [kb]
  (q kb :asignacion #(= (kget % :multiasignacion_id) nil) ))

(defn multiassignments
  [kb]
    (q kb :multiasignacion identity))

(defn assignments-from
  "
  (assignments-from 32)
  Devuelve una lista de objetos asignacion para la multiasignacion 32.
  "
  [kb multiid]
  (q kb :asignacion #(= (kget % :multiasignacion_id) (get-iid multiid))))


(defn multiasignacion?
  "Devuelve si un objeto es una multiasignacion
  si buscando un valor en :profesor_id devuevl :nohay."
  [obj]
  (= :nohay (get obj :profesor_id :nohay)))


(defn chorario-default
  "TODO deberia sacarse de la configuracion. Por ahorita esta bien."
  []
  {:lgs [{:lectures [{:t "t" :n 5 :d 1}]
          :days [0 1 2 3 4]
          :hours [8 9 10 11 12 13 16 17 18]
          }]
   :samehourtp false
   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn lg-add-constraint
  "Recibe un lg y le agrega una restriccion de horario, con lo cual devuelve
  un nuevo lg sin las horas y dias definidos en la restriccion constr.
  
  OJO: Las horas y las restricciones deben ser sets #{} . Dichos sets se deben
  crear antes de este paso para que todo funcione bien.
  "
  [lg constr]
  (assoc lg :hours (difference (get lg :hours) (get constr :nohours #{}))
            :days  (difference (get lg :days)  (get constr :nodays #{}))))


(defn lg-add-constraints
  "Aplica un conjunto de constraints a un lecture-group."
  [lg constrs]
  (if (empty? constrs)
    lg
    (recur (lg-add-constraint lg (first constrs))
           (rest constrs))))


(defn lgs-add-constraints
  "Aplica un conjunto de restricciones a todos los lecture-groups datos."
  [lgs constrs]
  (mapv #(lg-add-constraints % constrs) lgs))


(defn lg-lectures
  "Retorna los descriptures de lectures de un lecture-group como un vector."
  [lg]
  (get lg :lectures))

(defn lgs-base-index
  "Obtiene el indice base para los lecture-groups, el cual depende de si hay
  clases de teoria calendarizadas o no. Hay que buscarlas en los
  lecture-groups."
  [mb lgs]
  (if (and
        (empty?
          (filter (fn [lecture]
                    (= "p" (get lecture :t)))
                  (flatten
                    (mapv lg-lectures lgs))))
        (nil? (:gpid mb)))
    1 ; solo hay teoria [ET ...]
    2)) ; hay practica    [ET EP ...]


(defn lgs-indexes
  "Return a vector of lg's including the indexing data for locating bound
  members. Recibe el metabound y los lgs por separado."
  ([mb lgs shtp wdays]
   (lgs-indexes mb lgs shtp wdays (lgs-base-index mb lgs) []))
  ([mb lgs shtp wdays cidx nlgs]
   (if (empty? lgs)
     nlgs
     (let [;wdays    (configuration kb :dias_laborales)
           noindex   -1   ; TODO significa que no hay indice
           lg        (first lgs)
           nlectures (if shtp
                       (get (first (get lg :lectures)) :n)
                       (reduce + (map :n (get lg :lectures))))
           typn      (count (distinct (map :t (:lectures lg))))
           typ       (if (> typn 1) true false)
           ndays     (count (lg :days))
           hidx      cidx
           dio       (:daysinorder lg)
           pidx      (if (or (> ndays nlectures)
                             typ)
                      (inc hidx)
                       noindex)
           ; TODO aqui hay un problema porque las restricciones aplicadas
           ; a estos lgs podrian haberle quitado dias disponibles y no
           ; habria suficientes dias para dar todas las clases.
           ; ASI NO SE PUEDE CALENDARIZAR.
           einsuf    (if (< ndays nlectures)
                       (println "Dias insuficientes para las clases. "
                                ndays ">" nlectures))
           ncidx      (inc (max hidx pidx)) ]
       (recur
         mb 
         (if shtp
                (if (not (empty? nlgs))
                  '()
                  (rest lgs))
                (rest lgs))
         shtp
         wdays
         ncidx
         (conj nlgs (assoc lg :hidx hidx :pidx pidx)))))))

;(s/fdef lgs-indexes
;        :args (s/cat :lgs ::ch/lgs :shtp ::ch/samehourtp)
;        :ret ::lg/lgs) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn recursamientos
  "Devuelve un arreglo de recursadores que estan en una asignacion y que son
  de diferentes grupos."
  [kb aid]
  (map #(select-keys % [:grupo_id :alumnos])
       (q kb :recursamiento #(= (:asignacion_id %) aid))))


(defn mb-assignment
  "Devuelve los datos de una asignacion:
   {:teacher X :lecture X :group X :specialty X :assignment X}
  
  TODO: Agregar recursamientos en esta asignacion."
  [kb aid]
  (let [asi ($ kb :asignacion aid)
        lecid (asi :asignatura_id)
        speid (($ kb :asignatura lecid) :especialidad_id)]
    {:assignment (get-iid (asi :id))
     :teacher    (get-iid (asi :profesor_id))
     :group      (get-iid (asi :grupo_id))
     :recursamientos (recursamientos kb aid)
     :specialty  (get-iid speid)}))

(defn mb-assignments
  [kb maid]
  (mapv #(mb-assignment kb (% :id))
        (assignments-from kb maid)))

(defn get-constraints-for
  [kb massigns]
  (vector))



(defn gspace-spaces
  "Retorna los espacios de un gespacio a partir de su id."
  [kb gsid]
  (sons-tr kb :gespacio :espacio gsid))

(defn bige-spaces
  "Filtra un conjunto de espacios en un gespacio con id gsid y devuelve los
  espacios que son suficientemente grandes para soportar la cantidad gsize"
  [kb spaces size]
  (let [capacidad (fn [x] (Long. (get x :capacidad)))
        overpobd  (configuration kb :sobrepoblacion_max)
        maxcap    (apply max (map capacidad spaces))
        overpob   (if (nil? overpobd) 3 overpobd)
        capaces   (filter #(>=  (+ overpob (capacidad %)) size)
                          spaces)]
    (if (empty? capaces)
      (filter #(= (capacidad %) maxcap) spaces)
      capaces)))

; (bige_spaces (gspace-spaces 2) 45)


(defn en-spaces
  "Checa si los espacios son lo suficientemente grandes para el tamanio
  enviado. Si no, agarra los que no sean 5 alumnos mas pequenios que el
  mas grande.
  TODO: 5 parece un numero arbitrario.
  "
  [spaces size]
  (let [maxcap (transduce (map :capacidad) max 0 spaces)
        mincap (- maxcap 5)]
    (if (<= size maxcap)
      (filter #(>= (:capacidad %) size) spaces)
      (filter #(>= (:capacidad %) mincap) spaces))))


(defn resources-for-space
  [kb spid]
  (sons-tr kb :espacio :recurso spid))

(defn equipped-spaces
  "Filtra un conjunto de espacios y devuelve los espacios que estan equipados
  con los recursos requeridos segun los metabounds.
  
  TODO: Ahorita no hace nada !!!! ARREGLAR."
  [spaces resources]
   spaces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn total-students
  "Calcula la cantidad de estudiantes a partir de las asignaciones con
  diferentes grupos (multiasignaciones en las que se unen grupos)."
  [kb assignments]
  (let [groupids (distinct (map :group assignments))]
    (reduce + (map #(Long. (get ($ kb :grupo %) :alumnos)) groupids))))

(defn resources-for
  "Devuelve los recursos requeridos para este objeto asignacion o
  multiasignacion.
  TODO: Crear tabla multiasignacionrecurso e IMPLEMENTAR."
  [kb obj]
  (if (not (multiasignacion? obj))
    (sons-tr kb :asignacion :recurso (:id obj))  
    []))

(defn b-spaces
  "Retorna bounds iniciales a partir de los gespacios en la
  asignacion/multiasignacion. Tambien recibe la cantidad de estudiantes y
  los recursos requeridos. Las discapacidades tambien se envian aunque
  todavia no hay capturadas. "
  [kb gtid gpid students rresources discaps]
  (let [formatt (fn [x] (Long. (get x :id)))
        opes    (fn [x] (mapv formatt
                              (equipped-spaces
                                (bige-spaces kb (gspace-spaces kb x) students)
                                rresources)))
        tspaces (opes gtid)]
    (if (not (nil? gpid))
      (vector tspaces (opes gpid))
      (vector tspaces))))


(defn mb-spaces
  [kb gid students rresources discaps]
  (let [spaces (gspace-spaces kb gid)
        enspaces (en-spaces spaces students)
        eqspaces (equipped-spaces enspaces rresources) ]
    (if (> (count eqspaces) 0)
      eqspaces
      enspaces)))

(defn bound-spaces
  [mbspaces]
  (mapv #(-> % :id (Long.)) mbspaces))

(defn metabound
  "Crea un metabound de una asignacion o multiasignacion.
  Se necesta sacar:
  - asignaciones
  - tamanio del grupo completo (puede ser muy grande en las multiasignaciones)
  - restricciones aplicables a este metabound
    -> guardarlas en el metabound
  - sacar los espacios de teoria
  - sacar los espacios de practica
  - obtener las lgs
  - mejorar las lgs
  - dejar COMPLETO
  "
  [kb obj]
  (let [wdays     (configuration kb :dias_laborales)
        oid       (get-iid (kget obj :id))
        ismulti   (multiasignacion? obj)
        massigs   (if ismulti
                    (mb-assignments kb oid)
                    (vector (mb-assignment kb (kget obj :id))))
        recurs (if ismulti
                   (set (reduce concat
                                (map :recursamientos massigs)))
                   (:recursamientos (first massigs)))
        chorario  (if (empty? (kget obj :chorario))
                    (chorario-default)
                    (json/read-str (kget obj :chorario) :key-fn keyword))
        lgs       (kget chorario :lgs)
        shtp      (kget chorario :samehourtp)
        constrs   (get-constraints-for kb massigs)
        rreso     (resources-for kb obj)
        pmb       (hash-map
                    :multiassignment (if ismulti oid nil)
                    :mid (if ismulti oid nil)
                    :aid (if (not ismulti) oid nil)
                    :assignments massigs
                    :students (total-students kb  massigs)
                    :recursamientos recurs
                    ;:lgs (lgs-add-constraints lgs constrs)
                    :samehourtp shtp
                    :gtid (get-iid (kget obj :gespaciot_id))
                    :gpid (get-iid (kget obj :gespaciop_id))
                    :rresources rreso
                    :rdissabs []  ; TODO discapacidades IMPLEMENTAR
                    )
        ]
    (-> pmb
        (assoc :lgs (lgs-indexes pmb
                                 (lgs-add-constraints lgs constrs) shtp wdays)) )))

(defn agregar-recursamientos
  "Recibe la kb y los metabounds. Busca cada recursamiento en la kb y para
  cada asignacion del grupo con recursadores mete la info del recursamiento."
  [kb mbs]
  nil)

(defn partial-bounds
  "Recibe los lgs mejorados y genera el vector de horas, tomando en cuenta el
  vector

  lgs: [
    {:lectures [{:t \"t\" :n 5, d: 1 }]
     :hours [8 9 10 11 12 13 15 16 17 18]
     :days [0 1 2 3 4] }
    {:lectures [{:t \"p\" :n 2, d: 1 }]
     :hours [8 9 10 11 12 13 15 16 17 18]
     :days [0 1 2 3 4]}
  ]"
  ([lgs]
   (partial-bounds lgs []))
  ([lgs pbounds]
   (if (empty? lgs)
     pbounds
     (let [noindex -1
           lg      (first lgs)]
       (recur (rest lgs)
              (let [pb (conj pbounds (vec (lg :hours)))]
                (if (> (lg :pidx) noindex)
                  (conj pb (vec (lg :days)))
                  pb)))))))


(defn bounds
  [kb mb]
  (let [lgs (get mb :lgs)
        theory (get mb :theory)
        practice  (get mb :practice)
        spaces    (b-spaces
                    kb
                    (mb :gtid)
                    (mb :gpid)
                    (mb :students)
                    (mb :rresources)
                    (mb :rdisabs))
       fs (partial filterv some?) ]
    (fs (concat
          spaces
          (mapcat (fn ex-hours-days [lg]
                    (let [lectures (:lectures lg)
                          perm? (> (:pidx lg) -1)]
                      (vector (:hours lg)
                              (if perm? (set (:days lg))))))
                  lgs))) ))

    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLLISION DETECTION
;; - Primero propias
;; - Segundo entre dos bounds
;; * Problema: UNa permutacion existe cuando hay menos clases que los
;; dias de la semana O cuando hay mas de un tipo de clase (t/p) en un
;; :lectures (muchos lecture). ARREGLAR!!!



;; END COLLISION DETECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OLD BOUNDS

(defn ltheory?
  "Dice si una lectura es teoria."
  [lecture]
  (= (get lecture :t) "t"))

(defn lpractice?
  "Dice si una lectura es practica."
  [lecture]
  (= (get lecture :t) "p"))

(defn lectures-from
  [lgs pred]
  (filterv #(not (empty? %))
           (mapv #(filterv pred %)
                 (map :lectures lgs))))


(defn ltheories
  [lgs]
  (lectures-from lgs ltheory?))

(defn lpractices
  [lgs]
  (lectures-from lgs lpractice?))


(defn omb-tmhi?
  "Dice si todas las horas de teoria son en la misma hora de inicio."
  [lgs]
  (let [ntheories (reduce + (map count (ltheories lgs)))]
    (if (>  ntheories 1)
      false
      true)))

(defn omb-pmhi?
  "Dice si todas las horas de teoria son en la misma hora de inicio."
  [lgs]
  (let [npractices (reduce + (map count (lpractices lgs)))]
    (if (>  npractices 1)
      false
      true)))

(defn omb-gmhi?
  [lgs]
  (if (and (= 1 (count lgs))
           (let [lectures (get (first lgs) :lectures)]
             (and (some? (some ltheory? lectures))
                  (some? (some lpractice? lectures)))))
    true
    false))

(defn omb-tp
  "Extrae el arreglo de teoria para los old-metabounds."
  [lgs cual wdays]
  ;  (let [lectures (flatten (map :lectures lgs))]
  ;    (mapv #(assoc % :nohours [] :nodays [])
  ;          (filterv #(= (get % :t) cual) lectures))))
  (let [;wdays (configuration kb :dias_laborales)
        whours [8 9 10 11 12 13 16 17 18]
        pred #(= cual (get % :t))]
    (filterv some?
           (flatten
             (mapv #(map (fn [x]
                           (if (pred x)
                             (let [days (get %2 :days)
                                   hours (get %2 :hours)]
                               (assoc x :nohours (vdifference whours hours)
                                        :nodays (vdifference (range 0 wdays) days)
                                        :days days
                                        :hours hours ))))
                         %1)
                   (mapv :lectures lgs)
                   lgs)))))


(defn omb-theory
  "Extrae el arreglo de teoria para los old-metabounds."
  [lgs]
  (omb-tp lgs "t"))

(defn omb-practice
  "Extrae el arreglo de teoria para los old-metabounds."
  [lgs]
  (omb-tp lgs "p"))

(defn omb-gperm?
  "Dice si hay una gperm segun los lgs y el ghmi."
  [teo pra gmhi]
  (if gmhi
    (< (reduce + (mapv :n (concat teo pra))) 5)
    false))



(defn awdays?
  "All Working days? Dice si se cumplen todos los dias laborales "
  [kb days]
  (let [wdays (configuration kb :dias_laborales)]
    (= days wdays)))


(defn lgs-stats
  "
  {:lgs [
  {:lectures [
  {  .. \"t\"   }  
  {  .. \"p\"   }
  ]
  }
  }

  "
  [lgs cual]
  (cond

    ; Numeros
    (= cual :tlg1)  ; Theory on lg 1
    (count (filter ltheory? ((first lgs) :lectures)))

    (= cual :tlg2)  ; Theory on lg 2
    (count (filter ltheory? ((second lgs) :lectures)))

    (= cual :tlgr)  ; Theory on rest of lgs
    (count (filter ltheory? (flatten (map :lectures (rest lgs)))))

    (= cual :plg1)  ; Practice on lg 1
    (count (filter lpractice? ((first lgs) :lectures)))

    (= cual :plg2)  ; Practice on lg 2
    (count (filter lpractice? ((second lgs) :lectures)))

    (= cual :plgr)  ; Practice on rest of lgs
    (count (filter lpractice? (flatten (map :lectures (rest lgs)))))

    (= cual :dtlg1) ; Days of theory on lg1
    (reduce + (mapv :n (filter ltheory? ((first lgs) :lectures))))

    (= cual :dplg1) ; Days of practice on lg1
    (reduce + (mapv :n (filter lpractice? ((first lgs) :lectures))))

    (= cual :dplg2) ; Days of practice on lg2
    (reduce + (mapv :n (filter lpractice? ((second lgs) :lectures))))


    ; Predicados

    (= cual :mtlg1) ; Many theorys on lg 1
    (> (count (filter ltheory? ((first lgs) :lectures)))
       1)

    (= cual :mplg1) ; Many practices on lg 1
    (> (count (filter ltheory? ((first lgs) :lectures)))
       1)

    

    (= cual :tplg1) ; Theory and practice on lg1
    (and
      (> (+ (lgs-stats lgs :tlg1) (lgs-stats lgs :plg1))
         0)
      (= (+ (lgs-stats lgs :tlgr) (lgs-stats lgs :plgr))
         0))



    true nil  ))

(defn omb-detect-case
  [mb]
  (let [shtp (get mb :samehourtp)
        lgs  (get mb :lgs) ]
    (cond
      ; :normal
      ; [{:lectures [{N :t }] }]
      ; :gmhi false :tmhi :false :pmhi false
      ; :gperm false :tperm (Depende de N) :pperm false
      (and (not shtp)
           (= (count lgs) 1)
           (= (lgs-stats lgs :tlg1) 1)
           (= (lgs-stats lgs :plg1) 0)
           (= (lgs-stats lgs :tlgr) 0)
           (= (lgs-stats lgs :plgr) 0) )
      (assoc mb :case :normal
                :gmhi false :tmhi false :pmhi false
                :gperm false :pperm false
                :tperm (if (awdays? (get (first (get (first lgs) :lectures)) :n))
                         false
                         true))

      ; :shtp
      ; [{:lectures [{N :t }
      ;              {M :p }]}]
      ; :gmhi false :tmhi :false :pmhi false
      ; :gperm (Depende de N) :tperm fasle :pperm false
      (and shtp
           (= (count lgs) 1)
           (= (lgs-stats lgs :tlg1) 1)
           (= (lgs-stats lgs :plg1) 1)
           (= (lgs-stats lgs :tlgr) 0)
           (= (lgs-stats lgs :plgr) 0))
      (assoc mb :case :shtp
                :gmhi false :tmhi false :pmhi false
                :tperm false :pperm false
                :gperm (if (awdays?
                             (+ (lgs-stats lgs :dtlg1)
                                (lgs-stats lgs :dplg1)))
                         false
                         true))

      ; :tt
      ; [{:lectures [{T} ]}
      ;  {:lectures [{T} ]}
      ;  ]
      ; :gmhi false  :tmhi :false :pmhi false
      ; :gperm false :tperm :false :pperm :false
       (and (not shtp)
           (= (count lgs) 2)
           (= (lgs-stats lgs :tlg1) 1)
           (= (lgs-stats lgs :tlg2) 1)
           (= (lgs-stats lgs :plg1) 0)
           (= (lgs-stats lgs :tlgr) 1)
           (= (lgs-stats lgs :plgr) 0) )
      (assoc mb :case :tt
                :gmhi false :tmhi false :pmhi false
                :gperm false :pperm false :tperm  false)
      ; :typ
      ; [{:lectures [{N5 :t 1h} ]}
      ;  {:lectures [{M2 :p 1h} ]}]
      ; :gmhi false  :tmhi :false :pmhi false
      ; :gperm false :tperm (Dep N) :pperm (Dep M)
      (and (not shtp)
           (= (count lgs) 2)
           (= (lgs-stats lgs :tlg1) 1)
           (= (lgs-stats lgs :plg2) 1)
           (= (lgs-stats lgs :tlgr) 0)
           (= (lgs-stats lgs :plgr) 1))
      (assoc mb :case :typ
                :gmhi false :tmhi false :pmhi false
                :gperm false
                :tperm (if (awdays? (lgs-stats lgs :dtlg1))
                         false
                         true)
                :pperm (if (awdays? (lgs-stats lgs :dplg2))
                         false
                         true))

      ; :221
      ; [{:lectures [{N1 :t 1h}
      ;              {M2 :p 2h}]}]
      ; :gmhi true  :tmhi :false :pmhi false
      ; :gperm true :tperm false :pperm false
      (and (not shtp)
           (= (count lgs) 1)
           (= (lgs-stats lgs :tlg1) 1)
           (= (lgs-stats lgs :plg1) 1)
           (= (lgs-stats lgs :tlgr) 0)
           (= (lgs-stats lgs :plgr) 0))
      (assoc mb :case :221
                :gmhi true :tmhi false :pmhi false
                :gperm true :tperm false :pperm false)

      ; :221d
      ; tipico 2 2 1 (sin shtp) con horas de inicio distintas
      ; [{:lectures [{N1 :t }]}
      ;  {:lectures [{M2 :p 2h}]}]
      ; :gmhi false  :tmhi :false :pmhi false
      ; :gperm false :tperm (Dep N) :pperm (Dep M)
      (and (not shtp)
           (= (count lgs) 2)
           (= (lgs-stats lgs :tlg1) 1)
           (= (lgs-stats lgs :plg2) 1)
           (= (lgs-stats lgs :tlgr) 0)
           (= (lgs-stats lgs :plgr) 0))
      (assoc mb :case :221d
                :gmhi false :tmhi false :pmhi false
                :gperm false
                :tperm (if (awdays? (lgs-stats lgs :dtlg1)) false true)
                :pperm (if (awdays? (lgs-stats lgs :dplg2)) false true) )
      
      ;
; :341
; clase de alimentos con 2 laboratorios y 1 practica a la misma hora
; [{:lectures [{1 :t 1h}
;              {1 :p 3h}
;              {1 :p 4h}]}]
; :gmhi true  :tmhi :false :pmhi false
; :gperm true :tperm false :pperm false
;
; :341d
; clase de alimentos con 2 laboratorios (misma hora) y teoria a otra hora
; [{:lectures [{N :t 1h}]}
;  {:lectures [{M :p 3h}
;              {P :p 4h}]}]
; :gmhi false  :tmhi :false :pmhi true
; :gperm false :tperm (Dep N) :pperm true (diff p's)
;
; :34d1d
; clase de alimientos con 2 laboratorios (hora dif) y teoria (hora dif)
; [{:lectures [{N :t 1h}]}
;  {:lectures [{M :p 3h}]}
;  {:lectures [{P :p 4h}]}]
; :gmhi false  :tmhi :false :pmhi true
; :gperm false :tperm (Dep N) :pperm true (diff p's)


      :default (assoc mb :case :unsupported))))



(defn old-metabound
  "Retorna los viejos metabounds a partir de los bounds actuales.
  No estoy seguro si son compatibles, veremos.

  Parece que ya estan las Xmhi.
  Faltan las Xperm y los bounds. A ver que tan rapido sale. Para YA.
  "
  [mb]
  (let [
        lgs (mb :lgs)
        theory (omb-theory lgs)
        practice (omb-practice lgs)
        ;spaces (b-spaces
        ;        (mb :gtid)
        ;        (mb :gpid)
        ;        (mb :students)
        ;        (mb :rresources)
        ;        (mb :rdisabs))
        ;tspaces (first spaces)
        ;pspaces (or (second spaces) [])
         tspaces (if (some? (:gtid mb))
                   (bound-spaces
                     (mb-spaces (:gtid mb)
                                (:students mb)
                                (:rresources mb)
                                (:rdisabs mb)))
                   nil)
         pspaces (if (some? (:gpid mb))
                   (bound-spaces
                     (mb-spaces (:gpid mb)
                                (:students mb)
                                (:rresources mb)
                                (:rdisabs mb)))
                   nil)
        ]
    (assoc (omb-detect-case mb)
           :theory theory
           :practice practice
           :tspaces tspaces
           :pspaces pspaces
           )))


(defn chorario-random
  "Para probar cosas. Mejor que here que en otra parte. Ademas me ayuda a
  documentar casos raritos."
  ([cual]
   (nth
     [
      {:lgs [{:lectures ; :normal  0
               [{:t "t" :n 4 :d 1}]
              :hours #{8 9 10 11 12}  ; solo maniana
              :days #{0 1 2 4}        ; no el jueves
              }]
       :samehourtp false}
      {:lgs [{:lectures ; samehourtp 1
               [{:t "t" :n 5 :d 1}
                {:t "p" :n 5 :d 2} ]
              :hours #{8 9 10 11 12}  ; solo maniana
              :days #{0 1 2 4}        ; no el jueves
              }]
       :samehourtp true}
      {:lgs [{:lectures [{:t "t" :d 1 :n 5}] ; typ 2
              :days #{1 3}  ; x j
              :hours #{16 17 18} }
             {:lectures [{:t "p" :d 2 :n 2}]
              :days #{0 2 4}  ; l x v
              :hours #{8 9 10 11 12}}
             ]
       :samehourtp false}
      {:lgs [{:lectures [{:t "t" :n 1 :d 1}
                         {:t "p" :n 2 :d 2}]  ; 221
              :hours #{8 9 10 11 12 13 16 17 18}
              :days #{0 1 2 3 4} } ]
       :samehourtp false}
      {:lgs [{:lectures [{:t "t" :n 1 :d 1}] ; 221d
              :hours #{8 9 10 11 12 13 16 17 18}
              :days #{0 1 2 3} }
             {:lectures [{:t "p" :n 2 :d 2}]
              :hours #{8 9 10 11 12 13 16 17 18 19}
              :days #{0 1 2 3} }]
       :samehourtp false}

      ] 
     cual)))

(defn obounds
  [omb]
  (let [lgs (get omb :lgs)
        shtp (get omb :samehourtp)
        cas (get omb :case)
        theory (get omb :theory)
        practice  (get omb :practice)
       tspaces (:tspaces omb)
       pspaces (:pspaces omb)

       tspaceerr (if (empty? tspaces)
                   (do
                     (println "No hay espacios de teoria!!")
                     (println omb)))
       tspaceerr (if (and (some? pspaces) (empty? pspaces))
                   (do
                     (println "No hay espacios de practica!!")
                     (println omb)))
       fs (partial filterv some?)
        
        ]
    (cond
      (= cas :normal)
      (fs [tspaces
           (get (first theory) :hours)
           (if (get omb :tperm)
             (get (first theory) :days)) ] )

      (= cas :tt)
      (fs [tspaces
           (get (first theory) :hours)
           (get (first theory) :days)
           (get (second theory) :hours)
           (get (second theory) :days) ])


      (= cas :shtp)
      (fs [tspaces
           pspaces
           (get (first theory) :hours)
           (if (get omb :tperm)
             (get (first theory) :days)) ] )

      (= cas :typ)
      (fs [tspaces
           (get (first theory) :hours)
           (if (get omb :tperm)
             (get (first theory) :days))
           pspaces
           (get (first practice) :hours)
           (if (get omb :pperm)
             (get (first practice) :days)) ] )

      (= cas :221)
      (fs [tspaces
           pspaces
           (get (first theory) :hours)
           (get (first theory) :days) ] )


      (= cas :221d)
      (fs [tspaces
           (get (first theory) :hours)
           (if (get omb :tperm)
             (get (first theory) :days))
           pspaces
           (get (first practice) :hours)
           (if (get omb :pperm)
             (get (first practice) :days)) ] )

      true [])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn partir-osol-rosa1
  [ombs osol]
  (loop [combs ombs cosol osol solp []]
    (if (empty? combs)
      solp
      (let [omb (first combs)
            size (:size omb)]
        (recur
          (rest combs)
          (drop size cosol)
          (conj solp (vec (take size cosol))))))))

(defn convertir-osol-rosa
  [ombs sol]
  (let [psol (partir-osol-rosa1 ombs sol)
        fs (partial filterv some?)]
    (apply
      concat
      (mapv
        (fn [omb asol]
          (let [caso (:case omb)]
            (cond
              (= caso :normal)
              (fs [(get (get asol 0) 0)
                   (get (get asol 1) 0)
                   (if (:tperm omb)
                     (get asol 2)) ])

              (= caso :typ)
              (let [tperm? (:tperm omb)
                    pindex (if tperm? 3 2)]
                (fs [(get (get asol 0) 0)
                     (get (get asol 1) 0)
                     (if (:tperm omb)
                       (get asol 2))
                     (get (get asol pindex) 0)
                     (get (get asol (+ pindex 1)) 0)
                     (if (:pperm omb)
                       (get asol (+ pindex 2)))
                     ]))

              (= caso :tt)
              (fs [(get (get asol 0) 0)
                   (get (get asol 1) 0)
                   (get asol 2)
                   (get (get asol 3) 0)
                   (get asol 4) ])

              true
              :undefined

              )))
        ombs
        psol))))




(defn partir-osol
  "Parte un vector de vectores en vectores de vectores pero que contienen solo
  cierta cantidad de la solucion.
  TODO: Generalizar para utilizar con las permutaciones que afectan lgs."
  [ombs osol]
  (loop [combs ombs cosol osol solp []]
    (if (empty? combs)
      solp
      (let [omb (first combs)
            size (:size omb)]
        (recur
          (rest combs)
          (drop size cosol)
          (conj solp (vec (take size cosol))))))))

(defn crear-oasignacion-tespacio
  [omb asol caso]
  (cond
    (= caso :normal)
    (first asol)

    (= caso :tt)
    (first asol)

    (= caso :typ)
    (first asol)

    (= caso :221)
    (first asol)

    (= caso :221d)
    (first asol)

    (= caso :shtp)
    (first asol)))

(defn crear-oasignacion-pespacio
  [omb asol caso]
  (cond
    ; [et t tperm? ep p pperm?]
    (= caso :typ)
    (if (:tperm omb)
      (get 3 asol)
      (get 2 asol))

    ; [et ep hg gperm?]
    (= caso :shtp)
    (get 1 asol)

    ; [et ep hg gperm]
    (= caso :221)
    (get 1 asol)    
    
    true
    nil))



(defn crear-horas
  "Crea un conjunto de horas en formato anexo para los datos datos:
  TODO Esta mal!! tienes que mapear 2 veces: por horas y por dias o algo
  asi. Piensa bien. Este es de los buenos.

  {:tipo N (1 t 2 p 3 t/p)
   :dia N (0 1 2 3 4 5 6)
   :hora N
   :duracion N }"
  [tipo duracion dias hora]
  (mapv
    (fn [dia]
      (hash-map :tipo tipo
                :duracion duracion
                :dia dia
                :hora hora))
    dias))


(defn crear-oasignacion-horas
  [omb asol caso]
  (cond
    (= caso :normal)
    (let [theory (:theory omb)
          h (second asol)
          t1 (first theory)
          d  (:d t1)
          n  (:n t1)
          ds (if (:tperm omb)
               (get asol 2)
               (:days t1))
          ]
      (crear-horas 1 d ds h)) ; tipo 1 es teoria

    (= caso :shtp)
    []

    ; [et t tperm? ep p pperm?]
    (= caso :typ)
    (let [theory    (:theory omb)
          practice  (:practice omb)
          tperm?    (:tperm omb)
          pindex    (if (:tperm omb) 3 2)
          pperm?    (:pperm omb)

          t1 (first theory)
          th (second asol)
          td  (:d t1)
          tn  (:n t1)
          tds (if tperm? (get asol 2) (:days t1))

          p1 (first practice)
          ph (get asol pindex)
          pd  (:d p1)
          pn  (:n p1)
          pds (if pperm? (get asol (+ pindex 2)) (:days p1))

         pp1 (println tperm? td tds th)
         pp2 (println asol pperm? (+ pindex 2))
         ]
      (vec
        (concat
          (crear-horas 1 td tds th)
          (crear-horas 1 pd pds ph))))

    ;[et ht1 pt1 ht2 pt2]
    (= caso :tt)
    (let [
          theory    (:theory omb)
          t1        (first theory)
          t2        (second theory)
         ]
      (vec
        (concat
          (crear-horas 1 (:d t1) (get asol 2) (get asol 1))
          (crear-horas 1 (:d t2) (get asol 4) (get asol 3)))))

    (= caso :221)
    []

    true
    (println "Caso no identificado")))


(defn creador-oasignacion
  "Crea el horario para una asignacion especificamente a partir de un
  cacho de solucion."
  [omb asol]
  (let [caso (:case omb)
        ;pp (println caso omb asol)
        asignacion_id (:aid omb)
        multiasignacion_id (:mid omb)
        espaciot_id (crear-oasignacion-tespacio omb asol caso)
        espaciop_id (crear-oasignacion-pespacio omb asol caso)
        ]
    {:multiasignacion_id multiasignacion_id
     :asignacion_id asignacion_id
     :espaciot_id espaciot_id
     :espaciop_id espaciop_id
     :clases (crear-oasignacion-horas omb asol caso)
     }))


(defn creador-ohorario
  "Recibe los ometabounds y una osol y devuelve un horario en formato
  anidado para ser enviado a php y que cree el horario."
  [ombs osol]
  (mapv #(creador-oasignacion %1 %2) ombs (partir-osol ombs osol)))




(defn osolucion-random
  "Crea una solucion default usando la informacion de los metabounds, ya que
  las tperms y pperms se manejan como los mismos vectores."
  [ombs obds]
  (mapcat
    (fn
      [omb obd]
      (let [caso (:case omb)
            p (println :normal)]
        (cond
          (= caso :normal)
          (filterv some?
                   [(rand-nth (get obd 0))
                    (rand-nth (get obd 1))
                    (if (:tperm omb)
                      (shuffle (get obd 2)))])

          ; [et t tp? ep p pperm?]
          (= caso :typ)
          (let [pindex (if (:tperm omb) 3 2)
                p (println :typ)
                p2 (println obd)]
            (filterv some?
                     [(rand-nth (get obd 0))
                      (rand-nth (get obd 1))
                      (if (:tperm omb)
                        shuffle (get obd 2))
                      (rand-nth (get obd pindex))
                      (rand-nth (get obd (+ pindex 1)))
                      (if (:pperm omb)
                        (shuffle (get obd (+ pindex 2))))]))
          
          true
          [] )))
    ombs obds))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn chorario-random-improved
  "Para probar cosas. Mejor que here que en otra parte. Ademas me ayuda a
  documentar casos raritos.
  Este tiene que randomizar tambien horas y dias, a fin de crear horarios que
  requieran permutaciones.
  gpid dice si hay teoria o no.
  "
  ([whours wdays gpid]
   (let [cual (if (nil? gpid)
                (rand-nth [0])
                (rand-nth [1 2 3 4]))
         lwd (count wdays)
         ri #(let [r (rand-int %)] (if (= 0 r) 1 r))
         ri1 (ri lwd)
         ri2 (ri lwd)
         days1 (take-rand ri1 wdays)
         days2 (take-rand ri2 wdays)
         hours1 (take-rand 7 whours)
         hours2 (take-rand 7 whours)

         ]
   (nth
     [
      {:lgs [{:lectures ; :normal               ; 0
               [{:t "t" :n ri1  :d 1}]
              :hours hours1
              :days days1
              }]
       :samehourtp false}
      {:lgs [{:lectures ; samehourtp            ;1
               [{:t "t" :n ri1  :d 1}
                {:t "p" :n 5 :d 1} ]
              :hours hours1
              :days days1 }]
       :samehourtp true}
      {:lgs [{:lectures [{:t "t" :d 1 :n ri1}] ; 2  typ
              :days days1
              :hours hours1}
             {:lectures [{:t "p" :d 1 :n ri2}]
              :days days2
              :hours hours2} ]
       :samehourtp false}
      {:lgs [{:lectures [{:t "t" :n 1 :d 1}; 3
                         {:t "p" :n 2 :d 2}]  ; 221
              :hours hours1
              :days days1 } ]
       :samehourtp false}                   ;4
      {:lgs [{:lectures [{:t "t" :n 1 :d 1}] ; 221d
              :hours hours1
              :days days1 }
             {:lectures [{:t "p" :n 2 :d 2}]
              :hours hours2
              :days days2 }]
       :samehourtp false}

      ] 
     cual))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREADOR DEL NUEVO HORARIO

(defn partir-sol
  "Parte un vector de vectores en vectores de vectores pero que contienen solo
  cierta cantidad de la solucion.
  TODO: Generalizar para utilizar con las permutaciones que afectan lgs."
  [mbs sol]
  (loop [cmbs mbs csol sol solp []]
    (if (empty? cmbs)
      solp
      (let [mb (first cmbs)
            size (:size mb)]
        (recur
          (rest cmbs)
          (drop size csol)
          (conj solp (vec (take size csol))))))))

(defn partir-dias
  [odias cantidades]
  (loop [dias odias cant cantidades ndias []]
    (if (empty? cant)
      ndias
      (let [ncant (first cant)
            cuantos (count dias)
            cdias (take ncant dias)
            nndias (take-last (- cuantos ncant) dias) ]       
        (recur
          nndias
          (rest cant)
          (conj ndias cdias)))))) 



(defn crear-asignacion-horas-lg
  [mb asol lg]
  (let [hora (get asol (:hidx lg))
        dias (if (:daysinorder lg)
               (:days lg)
               (if (> (:pidx lg) -1)
                 (get asol (:pidx lg))
                 (:days lg)))]
    (map (fn [lecture ldias]
           (let [tipo (:t lecture) ]
             (crear-horas
               (cond (= tipo "t") 1
                     (= tipo "p") 2
                     (= tipo "tp") 3)
               (:d lecture)
               ldias
               hora  )))
         (:lectures lg)
         (partir-dias dias (map :n (:lectures lg))) )))


(defn crear-asignacion-horas
  [mb asol]
  ;      (crear-horas 1 d ds h)) ; tipo 1 es teoria
  ;                   [tipo duracion dias hora]
  (let [lgs (:lgs mb)
        ]
    (loop [lgs lgs horas []]
      (if (empty? lgs)
        (flatten horas)
        (recur
          (rest lgs)
          (conj horas
                (crear-asignacion-horas-lg mb asol (first lgs))))))))

  
(defn creador-asignacion
  "Crea el horario para una asignacion especificamente a partir de un
  cacho de solucion."
  [mb asol]
  (let [asignacion_id (:aid mb)
        multiasignacion_id (:mid mb)
        espaciot_id (if (some? (:gtid mb)) (first asol))
        espaciop_id (if (some? (:gpid mb)) (second asol))
        ]
    {:multiasignacion_id multiasignacion_id
     :asignacion_id asignacion_id
     :espaciot_id espaciot_id
     :espaciop_id espaciop_id
     :clases (crear-asignacion-horas mb asol)
     }))


(defn creador-horario
  "Recibe los metabounds y una sol y devuelve un horario en formato
  anidado para ser enviado a php y que cree el horario."
  [mbs sol]
  (mapv #(creador-asignacion %1 %2) mbs (partir-sol mbs sol)))



(defn crear-metabounds
  [url]
  (let [kb (json/read-str
             (slurp url) :key-fn keyword)
        scheduling-data (into []
                              cat
                              [(q kb :asignacion
                                  #(nil? (:multiasignacion_id %)))
                               (q kb :multiasignacion identity)])
        metabounds (mapv (partial metabound kb) scheduling-data)
        nbounds (mapv (partial bounds kb) metabounds)
        metabounds (mapv #(assoc %1 :size (count %2))
                         metabounds
                         nbounds)]
  {:metabounds
   (map #(select-keys %
                      [:samehourtp
                       :aid
                       :mid
                       :gpid
                       :gtid
                       :assignments
                       :size
                       :recursamientos
                       :lgs])
        metabounds)
   :bounds (mapcat identity nbounds)
   :spaces (zipmap (keys (kget kb :espacio))
                   (map #(select-keys % [:id :nombre :lat :lng])
                        (vals (kget kb :espacio))))
   }))

(defn rand-sol
  [bounds]
  (mapv #(if (set? %) % (rand-nth %)) bounds))


(defn guardar-horario
  [url idh horario]
  (let [ops {:form-params {:id idh
                           :horario (json/write-str horario)}}
        r @(http/post url ops)]
    (if (= (:status r) 200)
      (:body r))))
