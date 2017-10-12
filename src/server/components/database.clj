(ns server.components.database
  "Componente para la base de datos.

  Se busca que guarde la conexion en :db y el esquema en :schema, el cual se usaria
  para las interfaces CRUD base (Por hacer)."
  (:require [com.stuartsierra.component :as component]))

(defrecord Database [config
                     db]
 component/Lifecycle

 (start [component]
   (println ";; Starting Database")
   (assoc component :db config))
 (stop [component]
   (println ";; Stopping Database")
   (assoc component :db nil)))

(defn new-database
  [config]
  (if (some? (:classname config))
    (map->Database {:config config})
    (map->Database {:config  (assoc config
                                    :classname "com.mysql.cj.jdbc.Driver"
                                    :subprotocol "mysql")})))

