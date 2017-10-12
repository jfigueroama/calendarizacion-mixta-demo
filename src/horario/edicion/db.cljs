(ns horario.edicion.db
  (:require [clojure.spec :as s]))



(def db {
         :new-hcs []
         :new-has []
         :ui {
              :tab :grupo
              :usuario nil
              :hid nil
              }
         })

