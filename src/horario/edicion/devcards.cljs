(ns horario.edicion.devcards
  (:require   [reagent.core  :as reagent]
              [cljs-react-material-ui.core :as ui]
              [cljs-react-material-ui.reagent :as rui]
              [cljs-react-material-ui.icons :as ic]
              [clojure.string :as string]
              [horario.edicion.utils :as utils]
              [horario.edicion.horarios :as h])
  (:require-macros [devcards.core :as dc :refer [defcard defcard-rg]]))


(def state
  (reagent/atom
    {:contador 0}))


(def handlers
  {:assoc-in (fn assoc-in-hn
               [db [_ path value]]
               (assoc-in db path value))
   :inc (fn inc-hn
          [db _]
          (update-in db [:contador] inc))})


(defn dispatch
  [[ev & args]]
  (if-let [hn (get handlers ev)]
    (do
      (reset! state (hn @state [ev args]))
      (utils/log ev))))


(defcard-rg primera
  "Some docs"
  (fn [contador owner]
    [:span
     [:strong "Contador: " @contador]
     [:input {:value "+"
              :type "button"
              :on-click #(dispatch [:inc])}]])
  (reagent/atom (:contador @state)))
