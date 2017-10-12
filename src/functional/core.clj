(require '[cats.core :as m])
(require '[cats.monad.either :as ei])
(require '[cats.monad.exception :as ex])
(require '[clojure.spec :as s])
(require '[clojure.core.reducers :as r])

(defn eex
  [err exe f v]
  (if (map? err)
    err
    (hash-map :error err :exception exe :fn f :value v)))

(defn efmap
  "
  f function to apply
  v value, left, right or value.
  fo origin function (used for fapply)"
  ([f v fo]
  (if (not (ei/left? v))
    (let [nv (if (ei/right? v) v (ei/right v))
          r (ex/try-on (m/fmap f nv))]
      (if (ex/success? r)
        @r
        (ei/left (eex :exception r fo v))))
    (ei/left  (eex @v nil nil nil))))
  ([f v]
   (efmap f v f)))

(defn hola [x] (str "hola " x))
(s/fdef hola :args (s/cat :x string?) :ret string?)
(s/instrument-all)

(def ident (fn identity2 [x] x))

(efmap inc (ei/right nil))
(efmap inc (ei/right 1))
(efmap inc (ei/left "joder con el error"))
(efmap hola "hola")

(efmap hola (efmap hola (efmap ident "hola")))

(->> "hola" (efmap ident) (efmap hola) (efmap hola))

(->> 234
     (efmap ident)
     (efmap hola)
     (efmap hola))


(pprint 
(map #(efmap identity %)
     (mapv #(efmap hola %)
           [(ei/right "mundo") (ei/left "hola") (ei/right "hola")])) )

(filter ei/either? [(ei/right 1) (ei/left "error")])

(defn eapply
  [f & valus]
  (let [lfs (filter ei/left? valus)]
  (if (> (count lfs) 0)
    (ei/left (eex @(first lfs) nil nil nil))
    (efmap (partial apply f)
           (map #(if (ei/right? %)
                   (deref %)
                   %)
                valus)
           f))))

(eapply inc (ei/left "Problema"))
(eapply + (ei/right 2) (ei/right 3) (ei/right 5))


(reduce (partial eapply +) [(ei/right 1) (ei/right 3) (ei/right 4)])

(reduce (partial eapply +) [(ei/right 1) (ei/right 3) (ei/right 4) (ei/left "error")])

(reduce (partial eapply +) [1 2 3  4])

(reduce (partial eapply +) [1 2 3 (ei/right 5) 4])
(reduce (partial eapply +) [1 2 3 (ei/left "error en parametro") 4])


(efmap inc (ei/right 3))
(efmap inc 20)
(efmap inc nil)

(eapply + 1 2 3 nil 5)
(eapply + 1 2 3 (ei/right 5) 5)

(r/fold (partial eapply +) (mapv ei/right (range 0 10000000000)))

