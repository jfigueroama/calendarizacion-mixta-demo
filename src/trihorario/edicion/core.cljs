(ns trihorario.edicion.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [trihorario.edicion.db :as db]
            [trihorario.edicion.events :as events]
            [trihorario.edicion.subs :as subsc]
            [trihorario.edicion.views :as views]
            [trihorario.edicion.config :as config]
            [trihorario.edicion.utils :as utils]
            [devtools.core :as devtools]))

(defn dev-setup [mode]
  (println (str mode " mode"))
  (if (#{"dev" "devcards" :dev :devcards} mode)
    (devtools/install!)))

(defn mount-root []
  (if-let [app (.getElementById js/document "app")]
    (reagent/render [views/main-panel] app)))

(defn ^:export init []
  (if-let [app (.getElementById js/document "app")]
    (let [entidad (keyword (.getAttribute app "data-entidad"))
          hid (.getAttribute app "data-id")
          hnombre (.getAttribute app "data-nombre")
          usuario (.getAttribute app "data-usuario")
          ekey (.getAttribute app "data-ekey")
          mode (.getAttribute app "data-mode")
          data-url (.getAttribute app "data-data-url")
          phpbu (.getAttribute app "data-php-base-url")
          dbvacia (empty? @(re-frame/subscribe [:db]))]
      (enable-console-print!)
      (if dbvacia
        (do
          (dev-setup mode)
          (re-frame/dispatch-sync [:setup {:entidad entidad
                                           :hid hid
                                           :hnombre hnombre
                                           :usuario usuario
                                           :ekey ekey
                                           :php-base-url phpbu
                                           :data-url data-url
                                           :mode mode}])))
      (if (not= mode "devcards")
        (mount-root)))))

(init)
