(ns translator.spec.lecture-group
  (:require [clojure.spec :as s]
            [translator.spec.lecture :as l]))


(s/def ::hour #{7 8 9 10 11 12 13 14 15 16 17 18 19})
(s/def ::hours (s/* (s/cat :hour ::hour)))
; (s/explain ::ts/lg-hours [9 10 11 12 17])

; 0 lunes 1 martes ... 6 domingo
(s/def ::day #{0 1 2 3 4 5 6})
(s/def ::days (s/* (s/cat :day ::day)))
; (s/explain ::ts/lg-days [3 4 5])

(s/def ::hidx (s/and integer? #(>= % 0)))
(s/def ::pidx (s/and integer?
                     (s/or :no-perm #{-1} :perm #(>= % 0))))

(s/def ::lecture-group
  (s/and (s/keys :req-un [::l/lectures ::hours ::days ::hidx ::pidx])))

(s/def ::lgs (s/+ ::lecture-group))


