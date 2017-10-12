(ns horario.edicion.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [horario.edicion.db :as db]
            [horario.edicion.events :as events]
            [horario.edicion.subs :as subsc]
            [horario.edicion.views :as views]
            [horario.edicion.config :as config]
            [horario.edicion.utils :as utils]
            [horario.edicion.devcards :as dc]
            [devtools.core :as devtools]))

(defn dev-setup [mode]
  (enable-console-print!)
  (println (str mode " mode"))
  (devtools/install!))

(defn mount-root []
  (if-let [app (.getElementById js/document "app")]
    (reagent/render [views/main-panel] app)))

(defn ^:export init []
  (if-let [app (.getElementById js/document "app")]
    (let [hid (.getAttribute app "data-id")
          usuario (.getAttribute app "data-usuario")
          mode (.getAttribute app "data-mode")]
      (dev-setup mode)
      (utils/log "Abierto el init y la carga de nuevos datos. Horario " hid)
      (re-frame/dispatch-sync [:inicializar-db hid usuario])
      (re-frame/dispatch-sync [:cargar-datos hid])
      (re-frame/dispatch-sync [:crear-socket hid])
      (if (not= mode "devcards")
        (mount-root)))))

