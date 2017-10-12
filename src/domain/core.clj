(ns domain.core
  (:require [hugsql.core :as h]
            [clojure.string :as st]
            [clojure.java.jdbc :as j] ))

(defn q
  ([dbc sql params]
  (h/db-run (:db dbc) sql params))
  ([dbc sql]
   (q dbc sql {})))

(defn exec
  ([dbc sql params]
   (h/db-run  (:db dbc) sql params :execute :affected))
  ([dbc sql]
   (exec dbc sql {})))

(defn insert
  ([dbc sql params]
   (h/db-run  (:db dbc) sql params :insert :returning-execute))
  ([dbc sql]
   (insert dbc sql {})))



(defn $
  "Localiza un registro o varios registros en una tabla por su id o ids."
  [dbc tabla id]
  (if (coll? id)
    (q dbc
       (str "SELECT * FROM :i:table WHERE id IN (:v*:ids)")
       {:table (name tabla) :ids id})
    (first (q dbc (str "SELECT * FROM " (name tabla) " WHERE id=:id") {:id id}))))

(defn transaction
  [tfn]
  (j/with-db-transaction
    (tfn)))
