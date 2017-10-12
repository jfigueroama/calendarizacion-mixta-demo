(ns client.group.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [reg-sub subscribe register-sub ]]
            [client.utils :as utils]
            [client.subs :as su]))


(defonce db re-frame.db/app-db)

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


(reg-sub
  :db
  (fn [db _]
    db))

(reg-sub
 :asignaciones
 su/asignaciones-sub)

; horarioasignaciones
(reg-sub
 :has
 su/has-sub)

; horarioclases
(reg-sub
 :hcs
 su/hcs-sub)

(reg-sub
 :carreras
 su/carreras-sub)

(reg-sub
  :grupos
  su/grupos-sub
  )

(reg-sub
  :grupo
  su/grupo-sub)


(reg-sub
  :profesores
  su/profesores-sub)

(reg-sub
 :asignaturas
 su/asignaturas-sub)


(reg-sub
  :gxcarrera
  :<- [:grupos]
  su/gxcarrera)

(reg-sub
  :hasxgrupo
  :<- [:has]
  su/hasxgrupo)


(reg-sub
  :hcsxha
  :<- [:hcs]
  su/hcsxha)

(reg-sub
  :hasyhcs
  (fn hasyhcs-sig-sub
    [[_ gid] _]
    [(subscribe [:hasxgrupo gid])
     (subscribe [:hcs])])
  su/hasyhcs)

(reg-sub :mprofesor su/mprofesor)
(reg-sub :masignatura su/masignatura)
(reg-sub :mespacio su/mespacio)
(reg-sub :mgrupo su/mgrupo)
(reg-sub :mhas su/mhorarioasignacion)
(reg-sub :mhcs su/mhorarioclase)

(reg-sub
  :tabla-horario-grupo
  (fn thg-signals-sub
    [[_ gid] _]
    (subscribe [:hasyhcs gid]))
  su/tabla-horario-grupo)

(reg-sub
  :seleccionados
  :<- [:hcs]
  su/seleccionados-sub)

(reg-sub
  :ha
  :<- [:mhas]
  (fn ha-sub
    [mhas [_ id]]
    (get mhas id)))


(reg-sub
  :hc
  :<- [:mhcs]
  (fn hc-sub [mhcs [_ id]]
    (get mhcs id)))


