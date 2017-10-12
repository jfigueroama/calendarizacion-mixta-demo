(ns translator.spec.lecture
  (:require [clojure.spec :as s]))


(s/def ::n  #{1 2 3 4 5})
(s/def ::d #{1 2 3 4 5})
(s/def ::t #{"t" "p"})

(s/def ::lecture (s/keys :req-un [::n ::d ::t]))

; (s/explain ::ts/lecture {:n 5 :d 1 :t "t"}) 


(s/def ::lectures (s/+ ::lecture))

; (s/explain ::ts/lectures [{:n 5 :d 1 :t "p"}])


