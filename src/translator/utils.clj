(ns translator.utils
  (:use [clojure.set]))

(defn tokeyword
  [valu]
  (if (not (keyword? valu))
    (if (string? valu)
      (keyword valu)
      (tokeyword (str valu)))
    valu))

(defn tonumber
  [valu]
  (if (not (number? valu))
    (if (string? valu)
      (try (Long. valu)
           (catch Exception e
             nil))
      (if (keyword? valu)
        (tonumber (name valu))
        (tonumber (str valu))))
    valu))

(defn k=
  [a b]
  "Devuelve si una clave a es igual a una clave b. Las claves se transforman
  a keywords para la comparacion."
  (let [ak (tokeyword a)
        bk (tokeyword b)]
    (= ak bk)))

(defn tostring
  [valu]
  (if (keyword? valu)
    (name valu)
    (if (not (string? valu))
      (str valu)
      valu)))

(defn kget
  "Obtiene un elemento del hashmap hsm a traves de una clave que puede
  interpretarse como string, keyword o numerica.
  TODO ROTO! arreglar.
  "
  ([hsm ke]
   (kget hsm ke nil [tokeyword tostring tonumber] nil))
  ([hsm ke defa]
   (kget hsm ke defa [tokeyword tostring tonumber] nil))
  ([hsm ke defa fns va]
   (if (empty? fns)
     (if (= va :NOHAY)
       defa
       va)
     (let [myfn (first fns)
           nva (get hsm (myfn ke) :NOHAY)]
       (if (not= :NOHAY nva)
         nva
         (recur hsm ke defa (rest fns) nva))))))

(defn ccontains?
  "Checa si un elemento está en una coleccion."
  [secu ele]
  (if (some #(= ele %) secu)
    true
    false))

(defn vdifference
  "Devuelve la diferencia de dos vectores "
    [sour toremove]
    (vec (difference (set sour) (set toremove))))

(defn assoc-in-coll
  "Funcion para asociar un valor a una clave pero dentro de una lista siempre. Si la lista
  ya existe se une el valor a la lista."
  [hs k v]
  (if (not (nil? k))
    (let [nv (or (kget hs k) '())
          fv (distinct (conj nv v))]
      (assoc hs k fv))
    hs))

(defn extract-key-to-index
  "
  Extrae un elemento de un arreglo como los de abajo y saca la salida senialada.
  (extract-key-to-index
   :teacher
  [{\"assignment\" 399, \"lecture\" 599, \"group\" 83, \"teacher\" 2, \"specialty\" 3, \"sameHourTP\" 0, \"fixedHours\" 0}
   {\"assignment\" 399, \"lecture\" 600, \"group\" 83, \"teacher\" 3, \"specialty\" 3, \"sameHourTP\" 0, \"fixedHours\" 0} ]
   5)
  =>
  {2 '(5) 3 '(5) }

  "
  [k mb ind]
  (apply (partial merge-with (comp distinct concat))
         (map #(assoc-in-coll {} (kget % k) ind) mb)))


(defn subtake
  "TODO hacerla eficiente"
  [coll begin n]
  (take n (nthrest coll begin)))


(defn take-rand
  [nmin v]
  (let [vlen (count v)
        maxremoved (- vlen nmin)
        totake (rand-int (inc maxremoved))
        vrlen (- vlen totake)
        trfn  (comp sort #(take vrlen %) shuffle)]
    (vec (trfn v))))

(defn rand-int-in
  "Devuelve un entero random entre mi y ma (exclusive)"
  [mi ma]
  (+ mi (rand-int (- ma mi))))


(defn get-iid
  "Obtener el id de una instancia pero en int de un string."
  [sid]
  (if (some? sid)
    (if (integer? sid)
      sid
      (if (keyword? sid)
        (get-iid (name sid))
        (int (Double. sid)))) ; se asume string si no int o keyword
    nil))


