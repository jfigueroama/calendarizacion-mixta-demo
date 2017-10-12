(ns client.group.views
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [reagent.core  :as reagent]
            [client.views :as cviews]
            [client.group.views.tabla :as tabla]
            
            [cljs-react-material-ui.core :as ui]
            [cljs-react-material-ui.reagent :as rui]
            [cljs-react-material-ui.icons :as ic]
            ))

(defn home-page []
  ;(ui/icon-button {:touch true} (ui/navigation-expand-more)))
  [rui/mui-theme-provider
   {:mui-theme (ui/get-mui-theme {:palette {:text-color (ui/color :blue200)}})}
   [rui/icon-button 
    {:on-click #(.alert js/window "clicked")}
    [ic/action-done ]]
    
    ])


(defn grupo-tabla-toggle
  [ops grupo gid]
  (let [mostrar (reaction (:_tabla @grupo))
        ncontenedor  (str "#" "grupo" gid)]
    (fn []
      [:span
       (if @mostrar
         [:a {:href ncontenedor
              :on-click #(dispatch-sync [:ocultar-tabla-grupo gid]) }
          "-tabla"]
         [:a {:href ncontenedor
              :on-click #(dispatch [:mostrar-tabla-grupo gid])}
          "+tabla"]) ])))

(defn grupo-data-toggle
  [ops grupo gid]
  (let [mostrar (reaction (:_data @grupo))
        ncontenedor  (str "#" "grupo" gid)]
    (fn []
      [:span
       (if @mostrar
         [:a {:href ncontenedor
              :on-click #(dispatch-sync [:ocultar-data-grupo gid]) }
          "-datos"]
         [:a {:href ncontenedor
              :on-click #(dispatch [:mostrar-data-grupo gid])}
          "+datos"]) ])))


(defn grupo [gid]
  (let [data (subscribe [:grupo gid])]
    [:div {:id (str "grupo" gid) :data-gid gid}
     [:span.tabla_titulo
        (str (:codigo @data) "(" gid ")     ") ]
     [:span.tabla_toggle [grupo-tabla-toggle {} data gid] ]
     " "
     [:span.tabla_toggle [grupo-data-toggle {} data gid] ]
     [:br]
     [:div [tabla/grupo-data {} data gid ] ]
     [:div [tabla/grupo-tabla {}  data gid] ]
     [:br]]))

(defn carrera [ops c]
  (let [grs (subscribe [:gxcarrera (:id c)])]
    (if (> (count @grs) 0)
      (into [:div
             ops
             [:h2 (:nombre c)]
              ]
            (for [gr @grs]
              ^{:key (str "cgrupo" gr)} [grupo gr]))
      nil)))

(defn carreras []
  (let [cas (subscribe [:carreras])]
    (fn []
      [:div (for [ca @cas]
              [carrera {:key (:id ca)} ca])])))


(defn main-panel []
  (fn []
    [:div
     ;[cviews/edition-panel]
     ;[carreras]
     
     ]))

