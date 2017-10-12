(ns client.group.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [client.group.db :as db]
            [client.events :as cevents]
            [client.group.events :as events]
            ;[client.subs :as su]
            [client.group.subs :as subsc]
            [client.group.views :as views]
            [client.group.config :as config]
            [client.utils :as utils]
            [devtools.core :as devtools]
            ) )



(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")
    (devtools/install!)))

(defn mount-root []
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (let [hid (.getAttribute (.getElementById js/document"app") "data-id") ]
    (if (some? hid)
      (do
        (dev-setup)
        (.info js/console "Abierto el init y la carga de nuevos datos.")
        (re-frame/dispatch-sync [:inicializar-db])
        (re-frame/dispatch-sync [:cargar-datos 1])  ; TODO horario id de donde sale?
        (re-frame/dispatch-sync [:crear-socket])
        (mount-root))
      (.error js/console "Nada por abrir. El horario no existe!"))))
