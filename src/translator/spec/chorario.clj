(ns translator.spec.chorario
  (:require [clojure.spec :as s]
            [translator.spec.lecture :as le]
            [translator.spec.lecture-group :as lg]))

(s/def ::samehourtp boolean?)

(s/def ::lecture-group
  (s/and (s/keys :req-un [::le/lectures ::lg/hours ::lg/days])))

(s/def ::lgs (s/+ ::lecture-group))




(s/def ::chorario (s/keys :req-un [::lgs ::samehourtp]))

