(ns horario.edicion.views
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [reagent.core  :as reagent]

            [cljs-react-material-ui.core :as ui]
            [cljs-react-material-ui.reagent :as rui]
            [cljs-react-material-ui.icons :as ic]

            [clojure.string :as string]

            [horario.edicion.utils :as utils]
            [horario.edicion.horarios :as h]))

(def tipo-horarioclase
  {:teoria          1
   :practica        2
   :teoriapractica  3
   :biblioteca      4
   :ingles          5
   :asesorias       6})

(defn mtp
  [cual son]
  [rui/mui-theme-provider
   {:mui-theme
    (ui/get-mui-theme
      (case cual
        :default {:palette {}}
        :blue {:palette {:text-color (ui/color :blue200)}}
        :dblue {:palette {:text-color (ui/color :red700)}}
        :red {:palette {:text-color (ui/color :red700)}}
        :green {:palette {:text-color (ui/color :green500)}}
        :yellow {:palette {:text-color (ui/color :yellow700)}}
        {:palette {}}))}
   son])

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
  [ui]
  [:span.panel-edicion ; -oculto
   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:cambiar-dia -1])
                :title "Decrementar día"}
               (ic/hardware-keyboard-arrow-left)]]
   [mtp :blue[rui/icon-button
              {:on-click #(dispatch [:cambiar-dia 1])
               :title "Incrementar día"}
              (ic/hardware-keyboard-arrow-right)]]

   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:cambiar-hora -1])
                :title "Decrementar hora de inicio"}
               (ic/hardware-keyboard-arrow-up)]]

   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:cambiar-hora 1])
                :title "Incrementar hora de inicio"}
               (ic/hardware-keyboard-arrow-down)]]

   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:cambiar-duracion 1])
                :title "Aumentar duración"}
               (ic/content-add-circle-outline)]]

   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:cambiar-duracion -1])
                :title "Decrementar duración"}
               (ic/content-remove-circle-outline)]]

   [mtp :green [rui/icon-button
                {:on-click #(dispatch [:enviar-cambios])
                 :title "Guardar"}
                (ic/action-done)]]

   [mtp :red [rui/icon-button
              {:title "Cancelar"
               :on-click #(dispatch [:cancelar-cambios])}
              (ic/navigation-cancel)]]

   [mtp :red [rui/icon-button
              {:on-click #(dispatch [:borrar])
               :title "Borrar clases"}
              (ic/action-delete)]]


   [mtp :yellow [rui/icon-button
                 {:on-click #(dispatch [:cargar-datos
                                        (.getAttribute
                                          (.getElementById js/document "app")
                                          "data-id")])
                  :title "Recargar datos"}
                 (ic/av-replay)]]

   [:br]

   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:deseleccionar])
                :title "Deseleccionar"}
               (ic/content-clear)]]

   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:cambiar-tipo 1])
                :title "Teoría"}
               (ic/communication-import-contacts)]]
   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:cambiar-tipo 2])
                :title "Práctica"}
               (ic/hardware-laptop)]]

   [mtp :blue [rui/icon-button
               {:on-click #(dispatch [:cambiar-tipo 3])
                :title "Teoría y Práctica"}
               (ic/communication-business)]]

   (if (= :grupo (:tab @ui))
     [mtp :blue [rui/icon-button
                 {:on-click #(dispatch [:cambiar-tipo 4])
                  :title "Biblioteca"}
                 (ic/maps-local-library)]])

   (if (= :grupo (:tab @ui))
     [mtp :blue [rui/icon-button
                 {:on-click #(dispatch [:cambiar-tipo 5])
                  :title "Inglés"}
                 (ic/action-language)]])
   (if (= :profesor (:tab @ui))
     [mtp :blue [rui/icon-button
                 {:on-click #(dispatch [:cambiar-tipo 6])
                  :title "Asesorias/Tutorias"}
                 (ic/action-supervisor-account)]])])




(defn tabs
   []
   (fn []
     [:span
      [rui/mui-theme-provider
       {:mui-theme (ui/get-mui-theme
                     {:palette
                      {:text-color (ui/color :blue200)
                       :background-color (ui/color :green700)
                       :ripple-color (ui/color :yellow200)}})}
       [rui/raised-button
        {:on-click #(dispatch [:tab :grupo])} "Grupos"]]

      [rui/mui-theme-provider
       {:mui-theme (ui/get-mui-theme
                     {:palette
                      {:text-color (ui/color :blue200)}})}
       [rui/raised-button
        {:on-click #(dispatch [:tab :profesor]) } "Profesor"]]

      [rui/mui-theme-provider
       {:mui-theme (ui/get-mui-theme
                     {:palette
                      {:text-color (ui/color :blue200)}})}
       [rui/raised-button
        {:on-click #(dispatch [:tab :espacio]) } "Espacio"]]]))









;;;;;;;;;

(defn home-page []
  ;(ui/icon-button {:touch true} (ui/navigation-expand-more)))
  [rui/mui-theme-provider
   {:mui-theme (ui/get-mui-theme {:palette {:text-color (ui/color :blue200)}})}
   [rui/icon-button
    {:on-click #(.alert js/window "clicked")}
    [ic/action-done]]])




(defn toggle
  [ops clase id cosa]
  (let [obj  (subscribe [clase id])
        ncontenedor  (str "#" (name clase) id)
        ncosa (subs (name cosa) 1)]
    (fn []
      (let [mostrar (cosa @obj)]
        [:span
         [:a {:href ncontenedor
              :on-click #(dispatch-sync [:toggle clase id cosa])}
          (if mostrar (str "-" ncosa) (str "+" ncosa))]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; GENERALES
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn entidad-celda
  [ops clase id {haid :haid hcid :hcid hora :hora}]
  (let [profesores  (subscribe [:mprofesor])
        asignaturas (subscribe [:masignatura])
        espacios    (subscribe [:mespacio])
        grupos      (subscribe [:mgrupo])
        cha (subscribe [:ha haid])
        chc (subscribe [:hc hcid])]
    (fn []
      (let [{nha :new oha :old} @cha
            {nhc :new ohc :old} @chc
            profesor (get @profesores (:profesor_id nha))
            asignatura (get @asignaturas (:asignatura_id nha))
            kespacio (if (= (:tipo nhc) 1)
                       :espaciot
                       (if (= (:tipo nhc) 2)
                         :espaciop))
            lespacios {:espaciot (get @espacios (:espaciot_id nha))
                       :espaciop (get @espacios (:espaciop_id nha))}
            grado (get {1 "Lic." 2 "Mtr." 3 "Dr."} (:grado profesor))
            noderivada  (= (:hora nhc) hora)
            tipoc (:tipo nhc)
            grupo   (get @grupos (:grupo_id nha))
            selected (true? (:_selected @chc))]
        ^{:key (str "tc-" (:id nha) "-" (:nd nhc))}
        [(utils/elem-clases
           :div
           [(if (:vector ops) :multicelda)])

         [(utils/elem-clases
            :span
            [:celda
             (if (not noderivada) "hora-extendida")
             (if selected "pe-seleccionado")
             (if (> tipoc (:teoriapractica tipo-horarioclase))
               (if selected "ic-selected" "ic-normal"))
             (if (or (not= nhc ohc)
                     (not= nha oha)) "pe-cambiado")
             (if (:_borrada nhc) :borrada)])
          {:data-horarioclase_id (:id nhc)
           :data-horarioasignacion_id (:id nha)
           :on-click #(dispatch [:seleccionar (:id nhc)])}
          (cond
            (and (= tipoc (:biblioteca tipo-horarioclase))
                 (= clase :grupo))
            [(utils/elem-clases :span [(if selected "ic-selected" "ic-normal")]) "Biblioteca"]
            (and (= tipoc (:ingles tipo-horarioclase))
                 (= clase :grupo))
            [(utils/elem-clases :span [(if selected "ic-selected" "ic-normal")]) "Inglés"]
            (and (= tipoc (:asesorias tipo-horarioclase))
                 (= clase :profesor))
            [(utils/elem-clases :span [(if selected "ic-selected" "ic-normal")]) "Asesorias/Tutorias"]
            (and (nil? (#{(:biblioteca tipo-horarioclase)
                          (:ingles tipo-horarioclase)
                          (:asesorias tipo-horarioclase)} tipoc)))
            [:div
             [(utils/elem-clases :span [(if selected :sel-asignatura :asignatura)])
              (:nombre asignatura)]
             (if (not= :profesor clase)
               [(utils/elem-clases :span [(if selected :sel-profesor :profesor)])
                [:br]
                (str grado " "(:nombres profesor) " " (:apellidos profesor))])
             (if (not= :grupo clase)
               [(utils/elem-clases :span [(if selected :sel-grupo :grupo)])
                [:br]
                (str (:codigo grupo))])
             (if (not= clase :espacio)
               [(utils/elem-clases
                  :span
                  [(cond (= tipoc 1)
                         (if selected :sel-teoria :teoria)
                         (= tipoc 2)
                         (if selected :sel-practica :practica)
                         (= tipoc 3)
                         (if selected :sel-typ :typ))])
                [:br]
                (if (some? kespacio)
                  (:nombre (kespacio lespacios))
                  (str (:nombre (:espaciot lespacios)) " | "
                       (:nombre (:espaciop lespacios))))])])]]))))



(defn entidad-tabla
  [ops clase id]
  (let [data  (subscribe [clase id])
        ;tabla (subscribe [:tabla-horario clase id])
        hasyhcs (subscribe [:hasyhcs clase id])
        nclase (subs (name clase) 1)]

    (fn []
      (if (nil? (:_tabla @data))
        (do
          nil)
        [(utils/elem-clases :div [(utils/visible (true? (:_tabla @data)))])
         (let [tabla (h/tabla-horario @hasyhcs clase id)]
           (doall
             [:table
              {:id (str "t" nclase id)}
               ;:class "table table-bordered table-striped"

              [:thead
               [:tr [:th.hora "HORARIO"] [:th "Lunes"] [:th "Martes"]
                [:th "Miércoles"] [:th "Jueves"] [:th "Viernes"]]]
              (into
                [:tbody]
                (for [hora (utils/khoras)]
                  (if (some some? (vals (get tabla hora)))

                    ^{:key hora}
                    (into [:tr [:td.hora (utils/hora2texto hora)]]
                          (for [dia (utils/kdias)]
                            (let [celda (get-in tabla [hora dia])
                                  colisiones (if celda (:colisiones celda))]

                              ^{:key (str dia hora)}
                              [(utils/elem-clases
                                 :td
                                 [(if (h/multicelda? celda)
                                    (if (:colisiones celda)
                                      :concol :sincol))])
                               (cond
                                 (nil? celda)
                                 nil
                                 (h/multicelda? celda)
                                 (for [c (:celdas celda)]
                                   ^{:key (:hcid c)}
                                   [entidad-celda {:vector true} clase id c])
                                 (not (h/multicelda? celda))
                                 [entidad-celda {} clase id celda]
                                 true
                                 [:span "ERROR"])]))))))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Panel edicion
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn entidad-data-panel
  [ops clase id]
  (let [data (subscribe [clase id])
        has  (subscribe [:hasx clase id])
        espacios (subscribe [:espacios])
        asignaturas (subscribe [:masignatura])
        grupos (subscribe [:mgrupo])
        profesores (subscribe [:mprofesor])
        new-has (subscribe [:new-has])
        new-hcs (subscribe [:new-hcs])
        asinh   (subscribe [ :asignaciones-sin-horario])]

    (fn []
      (if (nil? (:_datos @data))
        nil
        [(utils/elem-clases :div [(utils/visible (true? (:_datos @data)))])
         [:div
           ;"Asignaciones:"
          (let [aasinh (cond
                         (= clase :grupo)
                         (filter #(= (:grupo_id %) id) @asinh)
                         (= clase :profesor)
                         (filter #(= (:profesor_id %) id) @asinh))]
            (if (not (empty? aasinh)) ; No hay un ha para esta asignacion:
              (for [asi aasinh]
                ^{:key (str "asinh-" (:id asi))}
                [:div
                 (if (not (:_pendiente asi))
                   [:div
                    [:a {:href ""} "Asignacion sin horario "]

                    [rui/mui-theme-provider
                     {:mui-theme (ui/get-mui-theme
                                   {:palette
                                    {:text-color (ui/color :green700)}})}
                     [rui/icon-button
                      {:on-click
                       #(dispatch [:crear-ha
                                   (assoc asi
                                          :horario_id (js/parseInt (utils/get-hid))
                                          :asignacion_id (:id asi)
                                          :espaciot_id nil
                                          :espaciop_id nil)])
                       :title "Crear horario para esta asignación"}
                      (ic/content-add-circle)]]]
                   [:div [:span "Horario para asignación pendiente de guardar"] [:br]])])))
          [:br]
          (into
            [:div]
            (for [ha @has]
              ^{:key (str "edpa" id "-" (:id ha))}
              [:div#asignacion
                ;[:span (:id ha)]  ; TODO cambiar por link a sistema de horarios
               (let [profesor (get @profesores (:profesor_id ha))
                     grupo (get @grupos (:grupo_id ha))
                     asignatura (get @asignaturas (:asignatura_id ha))]


                 [:div
                   {:data-haid (:id ha)}
                   [:span (:anio ha) (:periodo ha) " "]
                   [:span.asignatura (:nombre asignatura)] " | "
                   [:span.profesor
                    (:nombres profesor) " " (:apellidos profesor)] " | "
                   [:span.grupo (:codigo grupo)]
                   [:span [:a {:href (str (utils/php-base-url)
                                          "?/domain/Asignacion/edit/"
                                          (:asignacion_id ha)) } "Editar"]]
                   (if (not (empty? (:comentario ha)))
                     [:span [:br] [:span.comentario (:comentario ha)]])])

               [:span "Teoría: "]
               (into
                 [:select
                  {:value (or (:espaciot_id ha) 0)
                   :on-change
                   #(dispatch [:cambiar-espacio (:id ha) :espaciot_id
                               (js/parseInt (-> % (.-target) (.-value)))])}

                  [:option {:value 0 } "-- Nada --"]]
                 (for [e @espacios]
                    ; TODO colorear segun si esta en el gespacio o no
                   ^{:key (str "edpa" id "-" (:id ha) "et" (:id e))}
                   [:option{:value (:id e) } (:nombre  e)]))

               [:span "Nuevas: " (count (filter #(and (= (:id ha) (:horarioasignacion_id %))
                                                      (= 1 (:tipo %))) @new-hcs))]
               [rui/mui-theme-provider
                {:mui-theme (ui/get-mui-theme
                              {:palette
                               {:text-color (ui/color :green700)}})}
                [rui/icon-button
                 {:on-click
                  #(dispatch [:crear-hc
                              {:tipo 1 :horarioasignacion_id (:id ha) :hora 8 :dia 0 :duracion 1}])
                  :title "Crear clase de teoría"}
                 (ic/content-add-circle)]]
                ;;
               [:span "Práctica: "]
               (into
                 [:select
                  {:value (or (:espaciop_id ha) 0)
                   :on-change
                   #(dispatch [:cambiar-espacio (:id ha) :espaciop_id
                               (js/parseInt (-> % (.-target) (.-value)))])}

                  [:option {:value 0 } "-- NADA --"]]

                 (for [e @espacios]
                   ^{:key (str "edpa" id "-" (:id ha) "ep" (:id e))}
                   [:option {:value (:id e) } (:nombre  e)]))

               [:span "Nuevas: " (count (filter #(and (= (:id ha) (:horarioasignacion_id %))
                                                      (= 2 (:tipo %) )) @new-hcs))]
               [rui/mui-theme-provider
                {:mui-theme (ui/get-mui-theme
                              {:palette
                               {:text-color (ui/color :green700)}})}
                [rui/icon-button
                 {:on-click
                  #(dispatch [:crear-hc
                              {:tipo 2 :horarioasignacion_id (:id ha) :hora 8 :dia 0 :duracion 1}])
                  :title "Crear clase de Práctica"}
                 (ic/content-add-circle)]]

               [:br]]))]]))))


              ;[rui/mui-theme-provider
              ;   {:mui-theme (ui/get-mui-theme
              ;                         {:palette
              ;                          {:text-color (ui/color :blue200)}})}

                 ;(into
                 ;  [rui/select-field
                 ;   { } ]
                 ;  (for [e @espacios]
                 ;    ^{:key (str "edpa" id "-" (:id ha) "e" (:id e))}
                 ;    (reagent/as-element [rui/menu-item
                 ;     {:label (:nombre e) :value (:id e)}]) ))]





(defn entidad-panel [clase id titulo]
  (let [data (subscribe [clase id])]
    (fn []
      (let []
        [:div {:id (str clase id) :data-id id}
         [:span.tabla_titulo
          (str titulo)]
         [:span.tabla_toggle [toggle {} clase id :_tabla]]
         " "
         [:span.tabla_toggle [toggle {} clase id :_datos]]
         [:br]
         [:div [entidad-data-panel {} clase id]]
         [:div [entidad-tabla      {} clase id]]
         [:br]]))))

;;;;;;;;;;;;;;;;; GRUPO

(defn carrera [ops c]
  (let [grs (subscribe [:gxcarrera (:id c)])]
    (if (> (count @grs) 0)
      (into [:div
             ops
             [:h2 (:nombre c)]]

            (for [gr @grs]
              ^{:key (str "cgrupo" (:id gr))}
              [entidad-panel :grupo (:id gr) (:codigo gr)]))
      nil)))

(defn carreras []
  (let [cas (subscribe [:carreras])]
    (fn []
      [:div (for [ca @cas]
              [carrera {:key (:id ca)} ca])])))





;;;;;;;;;;;;;;;;;; Profesores
(defn lista-profesores
  []
  (let [profesores (subscribe [:profesores])]
    (fn []
      (into
        [:div]
        (for [p @profesores]
          ^{:key (str "p" (:id p))}
          [entidad-panel
           :profesor
           (:id p)
           (str (utils/grado (:grado p))
                " " (:nombres p) " " (:apellidos p))])))))

;;;;;;;;;;;;;;;;; Grupos


(defn lista-grupos []
  (let [cas (subscribe [:carreras])]
    (fn []
      (into [:div]
            (for [ca @cas]
              ^{:key (str "carrera" (:id ca))}
              [carrera {:key (:id ca)} ca])))))

;;;;;;;;;;;;;;;;;; Espacios

(defn lista-espacios
  []
  (let [espacios (subscribe [:espacios-usados])]
    (fn []
      (into
        [:div]
        (for [e @espacios]
           ^{:key (str "espacio" (:id e))}
           [entidad-panel
            :espacio
            (:id e)
            (str (:nombre e)  " (" (:capacidad e) ")")])))))




;;;;;;;;;;;;;;;;;; MAIN

(defn main-panel []
  (let [ui (subscribe [:ui])]
    (fn []
      [:div.principal

       [:div.cabecera
       ;[cviews/edition-panel]
       ;[carreras]
        [:div [:span.titulo "Edición de Horarios CHILOS"] [:br] [tabs]]
        [edition-panel ui]]

       [:pre (str)]

       [:div.contenido
          [(utils/elem-clases :div [(utils/visible (= :grupo (:tab @ui)))])
           [lista-grupos]]
          [(utils/elem-clases :div [(utils/visible(= :profesor (:tab @ui)))])
           [lista-profesores]]
          [(utils/elem-clases :div [(utils/visible (= :espacio (:tab @ui)))])
           [lista-espacios]]]])))

