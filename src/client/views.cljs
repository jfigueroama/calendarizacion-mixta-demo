(ns client.views
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [reagent.core  :as reagent]
            
            [cljs-react-material-ui.core :as ui]
            [cljs-react-material-ui.reagent :as rui]
            [cljs-react-material-ui.icons :as ic]
            ))


(defn edition-panel
  "Pinta el panel de edicion para el grupo:
   
   Hora
   + (content-add)  Agrega una hora a las 7am el lunes de una hora
   X (content-clear)

   Operaciones en ese panel:
   Izq   Der   mover de dia (hardware-keyboard-arrow-left right)
   Arriba Abajo, mover de hora (hardware-keyboard-arrow-down up)
   + - (content-add-circle-outline remove)  Inc/Dec duracion 
   Guardar (action-done)  Cancelar (navigation-cancel)

  Este panel sale cuando se selecciona una celda y esto hace que todas
  las celdas seleccionables esten relacionadas por horarioasignacion_id.
  Las seleccionadas se guardan en 
  "
  [ops]
  (let [seleccionados (subscribe [:seleccionados])
        mhcs (subscribe [:mhcs]) ] ; hash
    (fn edition-panel-fn 
      []
      (let [nsel (count @seleccionados)
            shcs (map #(get @mhcs (:id %)) seleccionados)
            aid nil]
        [(if (or (= 0 (count @seleccionados)) true)
           :div.panel-edicion ; -oculto
           :div.panel-edicion)
;         [:div.pe-horarioasignacion
;          [:div "Asignación: " aid] [:br]
 ;         [:div "Clases: "] [:br]
 ;         [:div "Esp. Teoría: "] [:br]
 ;         [:div "Esp. Práctica: "] [:br] ]

;         [:div.pe-horarioasignacion
;          [:div "Asignación: " aid] [:br]
;          [:div "Clases: "] [:br]
;          [:div "Esp. Teoría: "] [:br]
;          [:div "Esp. Práctica: "] [:br] ]

   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:on-click #(dispatch [:cambiar-dia -1])
      :title "Decrementar día"}
     (ic/hardware-keyboard-arrow-left)] ]
 
   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:on-click #(dispatch [:cambiar-dia 1])
      :title "Incrementar día"}
     (ic/hardware-keyboard-arrow-right)] ]

   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:on-click #(dispatch [:cambiar-hora -1])
      :title "Decrementar hora de inicio"}
     (ic/hardware-keyboard-arrow-up)] ]

   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:on-click #(dispatch [:cambiar-hora 1])
      :title "Incrementar hora de inicio"}
     (ic/hardware-keyboard-arrow-down)] ]


   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:on-click #(dispatch [:cambiar-duracion 1])
      :title "Aumentar duración"}
     (ic/content-add-circle-outline)] ]

   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:on-click #(dispatch [:cambiar-duracion -1])
      :title "Decrementar duración"}
     (ic/content-remove-circle-outline)] ]


   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:on-click #(dispatch [:enviar-cambios])
      :title "Guardar"}
     (ic/action-done)] ]

   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:title "Cancelar"
      :on-click #(dispatch [:cancelar-cambios])}
     (ic/navigation-cancel)] ]

   [rui/mui-theme-provider
    {:mui-theme (ui/get-mui-theme
                  {:palette
                   {:text-color (ui/color :blue200)}})}
    [rui/icon-button
     {:on-click #(dispatch [:deseleccionar])
      :title "Deseleccionar"}
     (ic/content-clear)] ]


   ]))))
