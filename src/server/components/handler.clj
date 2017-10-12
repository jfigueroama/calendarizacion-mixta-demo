(ns server.components.handler
  "Define el componente webapp como las rutas y handlers de la aplicacion web.
  Requiere de la base de datos para integrar dicha info a los handlers."
  (:require [com.stuartsierra.component :as component]
            [server.handlers :as h]
            [compojure.core :refer :all]
            [compojure.route :as route])
  (:use ring.middleware.params
        ring.middleware.keyword-params
        ring.middleware.multipart-params
        ;ring.middleware.cookies
        ring.middleware.session
        ring.util.response))

(defn app-routes
  [conf comm dbc]
  (routes
    (GET "/" request h/demo-index)

    (GET "/ws/horarios/ihorarioasignaciones/:id"
       request (partial h/ihorarioasignaciones-handler conf dbc))
    (GET "/ws/horarios/ihorarioclases/:id"
       request (partial h/ihorarioclases-handler dbc))
    (GET "/ws/tabla/:tabla"
       request (partial h/tabla-handler dbc))
    (GET "/ws/horarios/data/:id"
       request (partial h/horario-datos-handler dbc))
    (GET "/ws/horarios/jdata/:id"
       request (partial h/jhorario-datos-handler dbc))
    
    (GET "/horario/edicion/devcards/:id"
       request (partial h/devcards-handler conf dbc))

    (GET "/horario/edicion/:id"
       request (partial h/horario-handler conf dbc))
 
    (GET "/trihorario/edicion/:entidad/:id"
       request (partial h/trihorario-handler conf dbc))

    (GET "/horario/llenar/:id"
       request (partial h/horario-llenar-hn conf dbc))

    (GET "/horario/metabounds/:id"
         request (partial h/horario-metabounds-hn conf dbc))

    ;(GET "/test" request (fn [rq] "hola mundo cruel y ya ching"))


    (GET  "/socket-horarios" req ((:rag comm)  req))
    (POST "/socket-horarios" req ((:rap comm)  req))

    (GET "/test"
         req
         (fn test-hn [req]
           "hola chicos feliz"))

    (route/files "/public/")))
     ; TODO incluir files




(defrecord Handler
  [config handler comm db]
  component/Lifecycle
  (start [component]
    (println ";; Starting Handler")
    (let [handler (-> (app-routes config
                                  (:comm component)
                                  (:db component))
                      wrap-keyword-params
                      wrap-params
                      wrap-session
                      wrap-multipart-params
                      ;wrap-cookies
                      )]
      (assoc component :handler handler)))
  (stop [component]
    (println ";; Stopping Handler")
    (assoc component :handler nil)))

(defn new-handler
  [conf]
  (map->Handler {:config conf}))
