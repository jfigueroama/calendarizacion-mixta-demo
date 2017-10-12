(ns trihorario.edicion.views
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [reagent.core  :as reagent]

            [cljs-react-material-ui.core :as ui]
            [cljs-react-material-ui.reagent :as rui]
            [cljs-react-material-ui.icons :as ic]

            [clojure.string :as string]

            [trihorario.edicion.utils :as utils]
            [trihorario.edicion.horarios :as h]))

(def tipo-horarioclase
  {:teoria          1
   :practica        2
   :teoriapractica  3
   :biblioteca      4
   :ingles          5
   :asesorias       6})

(defn alumnos-small
  [cap]
  [:span.panel-dato
   {:title "N. Alumnos"}
   [:span.capacidad-titulo-small "("]
   [:span.capacidad-small cap]
   [:span.capacidad-titulo-small ")"]])


(defn alumnos
  [cap]
  [:span.panel-dato
   {:title "N. Alumnos"}
   [:span.capacidad-titulo "A "]
   [:span.capacidad cap]])

(defn capacidad
  [cap]
  [:span.panel-dato
   {:title "Capacidad"}
   [:span.capacidad-titulo "C "]
   [:span.capacidad cap]])


(defn espacio-espacio
  "Pinta un indicador del tipo de clase de este espacio."
  [hc]
  (let [alterno (some? (:espacioa_id hc))
        ntipo (h/hc-tipo (:tipo hc)) ]
    [:span.espacio-espacio
     {:on-double-click
      #(dispatch [:abrir-espacioa
                  (:id hc)
                  (:espacioa_id hc)])
      :title
      (str (if alterno "Alterno ")
           (case ntipo
             :teoria "Teoría"
             :practica "Práctica"
             :teoriapractica "Teoría y Práctica"
             :biblioteca "Biblioteca"
             :ingles "Inglés"
             :asesorias "Asesorias"))}
     (str (if alterno "alt ")
          (case ntipo
            :teoria "T"
            :practica "P"
            :teoriapractica "TP"
            :biblioteca "B"
            :ingles "I"
            :asesorias "A"))]))


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
        :dgreen {:palette {:text-color (ui/color :green700)}}
        :yellow {:palette {:text-color (ui/color :yellow700)}}
        :fd-old {:palette {:text-color (ui/color :blue700)
                           :primary1Color (ui/color :blue700)}}
        :fd-new {:palette {:text-color (ui/color :orange500)
                           :primary1Color (ui/color :orange500)}}
        :colisiones {:palette {:text-color (ui/color :red700)}}
        :toggle {:palette {:text-color (ui/color :orange500)
                           :primary1Color (ui/color :orange500)}}
        :sin-cambios {:palette {:text-color (ui/color :blue200)}}
        :con-cambios {:palette {:text-color (ui/color :orange500)
                                :primary1Color (ui/color :orange500)
                                :secondary1Color (ui/color :orange500)}}
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
  []
  (let [ui (subscribe [:ui])
        seleccionados (subscribe [:seleccionados])
        borrados (subscribe [:borrados])
        diferentes (subscribe [:cantidad-diferentes])
        new-hcs (subscribe [:new-hcs])
        new-has (subscribe [:new-has])
        crs (subscribe [:cambios-recibidos-cantidad])
        cr  (subscribe [:cambio-recibido])]
    (fn []
      (let [nselecs (count @seleccionados)
            nbors (count @borrados)
            nhas (count @new-has)
            nhcs (count @new-hcs)]
        [:span.panel-edicion ; -oculto
         [mtp :blue [rui/icon-button
                     {:on-click #(dispatch [:cambiar-dia -1])
                      :title "Decrementar día"}
                     (ic/hardware-keyboard-arrow-left)]]
         [mtp :blue [rui/icon-button
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

         [mtp :green [:span
                      [rui/icon-button
                       {:on-click #(dispatch [:enviar-cambios])
                        :title "Guardar"}
                       (ic/action-done)]
                      (if (or (> (:has @diferentes) 0)
                              (> (:hcs @diferentes) 0)
                              (> nhas 0)
                              (> nhcs 0))
                        [rui/badge {:title "Asignaciones y clases modificadas"
                                    :badgeContent
                                    (str (+ nhas (:has @diferentes))
                                         ", "
                                         (+ nhcs (:hcs @diferentes)))}])]]

         [mtp :red [rui/icon-button
                    {:title "Cancelar"
                     :on-click #(dispatch [:cancelar-cambios])}
                    (ic/navigation-cancel)]]

         [mtp :red [:span
                    [rui/icon-button
                     {:on-click #(dispatch [:borrar])
                      :title "Borrar clases"}
                     (ic/action-delete)]
                    (if (> nbors 0)
                      [rui/badge {:title "Clases marcadas para eliminarse"
                                  :badgeContent nbors}])]]


         [mtp :yellow [rui/icon-button
                       {:on-click #(dispatch [:cargar-datos
                                              (:entidad @ui)
                                              (:hid @ui)])
                        :title "Recargar datos"}
                       (ic/av-replay)]]

         [:br]

         [mtp :blue [:span
                     [rui/icon-button
                      {:on-click #(dispatch [:deseleccionar])
                       :title "Deseleccionar"}
                      (ic/content-clear)]
                     (if (> nselecs 0)
                       [rui/badge {:badgeContent nselecs
                                   :title "Clases seleccionadas"}])]]

         [mtp :blue [rui/icon-button
                     {:on-click
                      #(dispatch [:cambiar-tipo (h/tipo-horarioclase :teoria)])
                      :title "Teoría"}
                     (ic/communication-import-contacts)]]
         [mtp :blue [rui/icon-button
                     {:on-click
                      #(dispatch [:cambiar-tipo
                                  (h/tipo-horarioclase :practica)])
                      :title "Práctica"}
                     (ic/hardware-laptop)]]

         [mtp :blue [rui/icon-button
                     {:on-click
                      #(dispatch [:cambiar-tipo
                                  (h/tipo-horarioclase :teoriapractica)])
                      :title "Teoría y Práctica"}
                     (ic/communication-business)]]

         (if (= :grupo (:entidad @ui))
           [mtp :blue [rui/icon-button
                       {:on-click 
                        #(dispatch [:cambiar-tipo 
                                    (h/tipo-horarioclase :biblioteca)])
                        :title "Biblioteca"}
                       (ic/maps-local-library)]])

         (if (= :grupo (:entidad @ui))
           [mtp :blue [rui/icon-button
                       {:on-click
                        #(dispatch [:cambiar-tipo 
                                    (h/tipo-horarioclase :ingles)])
                        :title "Inglés"}
                       (ic/action-language)]])
         (if (= :profesor (:entidad @ui))
           [mtp :blue [rui/icon-button
                       {:on-click 
                        #(dispatch [:cambiar-tipo 
                                    (h/tipo-horarioclase :asesorias)])
                        :title "Asesorias/Tutorias"}
                       (ic/action-supervisor-account)]])
         (if (= :espacio (:entidad @ui))
           [mtp :yellow [:span
                         " "
                         [rui/icon-button
                          {:on-click #(dispatch [:toggle-fdrawer])
                           :title "Filtros"}
                           (ic/action-search)] ]])
         [:span
           [mtp (if (> @cr 0) :con-cambios :sin-cambios)
            [rui/icon-button
               {:on-click
                #(dispatch [:toggle-cambios-drawer])
                :title (str "Log de cambios recibidos")}
               (ic/av-library-books)]]
           [mtp (if (> @cr 0) :con-cambios :sin-cambios)
             [rui/badge {:title "Cambios recibidos"
                         :badgeContent @crs}]] ]
         [mtp :yello
            [rui/icon-button
               {:on-click
                #(dispatch [:guardar-transit])
                :disabled true
                :title (str "Guardar horario estático")}
               (ic/content-save)]] ]))))




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
                       :espaciop (get @espacios (:espaciop_id nha))
                       :espacioa (get @espacios (:espacioa_id nhc))}
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
                (str (:codigo grupo) " ") [alumnos-small (:alumnos grupo)] ])
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
                  (if (:espacioa_id nhc)
                    [:span.alterno {:title "Espacio alterno"
                                    :on-double-click
                                    #(dispatch [:abrir-espacioa
                                                (:id nhc)
                                                (:espacioa_id nhc)])}
                     (str "alt. " (:nombre (:espacioa lespacios)))]
                    [:span {:on-double-click
                            #(dispatch
                               [:abrir-espacioa (:id nhc) nil])}
                    (:nombre (kespacio lespacios))])
                  [:span
                   {:on-double-click
                    #(dispatch
                       [:abrir-espacioa (:id nhc) nil])}
                   (str (:nombre (:espaciot lespacios)) " | "
                        (:nombre (:espaciop lespacios)))])]
               [espacio-espacio nhc])])]]))))



(defn entidad-tabla
  [ops clase id]
  (let [data  (subscribe [clase id])
        ;tabla (subscribe [:tabla-horario clase id])
        hasyhcs (if (not= :espacio clase)
                  (subscribe [:hasyhcs clase id])
                  (subscribe [:hasyhcs-espacio clase id]))
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
               [:tr [:th.hora "HORARIO "] [:th "Lunes"] [:th "Martes"]
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
          [:div
           "Agregar clases de biblioteca"
           ]
          [:hr]
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
                                          :horario_id (js/parseInt
                                                        (utils/get-hid))
                                          :asignacion_id (:id asi)
                                          :espaciot_id nil
                                          :espaciop_id nil)])
                       :title "Crear horario para esta asignación"}
                      (ic/content-add-circle)]]]
                   [:div
                    [:span "Horario para asignación pendiente de guardar"]
                    [:br]])])))
          [:br]
          (into
            [:div]
            (for [ha @has]
              ^{:key (str "edpa" id "-" (:id ha))}
              [:div#asignacion
               (let [profesor (get @profesores (:profesor_id ha))
                     grupo (get @grupos (:grupo_id ha))
                     asignatura (get @asignaturas (:asignatura_id ha))]
                 [:div
                   {:data-haid (:id ha)}
                   [:span (:anio ha) (:periodo ha) " "]
                   [:span.asignatura (:nombre asignatura)] " | "
                   [:span.profesor
                    (:nombres profesor) " " (:apellidos profesor)] " | "
                   [:span.grupo (:codigo grupo)] " "
                   (if (> (:alumnos ha) 0)
                     [alumnos (:alumnos ha)])
                   [:span " - " [:a {:href (str
                                             (utils/php-base-url)
                                             "/domain/Asignacion/edit/"
                                             (:asignacion_id ha)) } "Editar"]]
                   (if (not (empty? (:comentario ha)))
                     [:div [:pre.comentario (:comentario ha)]])])

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
                   [:option {:value (:id e) }
                            (str (:nombre  e) " - C: " (:capacidad e))]))

               [:span "Nuevas: " (count (filter #(and (= (:id ha) (:horarioasignacion_id %))
                                                      (= 1 (:tipo %))) @new-hcs))]
               [rui/mui-theme-provider
                {:mui-theme (ui/get-mui-theme
                              {:palette
                               {:text-color (ui/color :green700)}})}
                [rui/icon-button
                 {:on-click
                  #(dispatch [:crear-hc
                              {:tipo 1 :horarioasignacion_id (:id ha) :hora 8 :dia 0 :duracion 1
                               :espacioa_id nil}])
                  :title "Crear clase de teoría"}
                 (ic/content-add-circle)]]
                ;;
               [:br]
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
                   [:option {:value (:id e) }
                            (str (:nombre  e) " - C: " (:capacidad e))]))

               [:span "Nuevas: " (count (filter #(and (= (:id ha) (:horarioasignacion_id %))
                                                      (= 2 (:tipo %) )) @new-hcs))]
               [rui/mui-theme-provider
                {:mui-theme (ui/get-mui-theme
                              {:palette
                               {:text-color (ui/color :green700)}})}
                [rui/icon-button
                 {:on-click
                  #(dispatch [:crear-hc
                              {:tipo 2 :horarioasignacion_id (:id ha) :hora 8 :dia 0 :duracion 1
                               :espacioa_id nil}])
                  :title "Crear clase de Práctica"}
                 (ic/content-add-circle)]]

               [:br][:hr]]))]]))))


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

(defn entidad-datos-profesor
  [data]
  (let [cargo (subscribe [:cargo]) ]
    (fn []
      (into
        [:div.entidad-datos]
        (for [cid (:cargos @data)]
          (let [ca (get @cargo cid)]
            [:span.cargo
             {:title (:codigo ca)}
             (:nombre ca)]))))))



(defn entidad-datos-espacio
  [data]
  (let [recurso (subscribe [:recurso])]
    (fn []
      (into [:div.entidad-datos
             [capacidad (:capacidad @data)]]
            (for [rid (:recursos @data)]
              (let [re (get @recurso rid)]
                (if (= (:tipo re) (h/tipo-recurso :grupo))
                  ^{:key (str "recuso" (:id @data) "-" rid)}
                  [:span.panel-dato.recurso
                   (get (@recurso rid) :nombre)])))))))

(defn entidad-datos-grupo
  [data]
  (let []
    (fn []
      (into [:div.entidad-datos
             [alumnos (:alumnos @data)]]
            []))))

(defn entidad-datos
  [opts data clase id]
  (case clase
        :espacio
        [entidad-datos-espacio data]
        :profesor
        [entidad-datos-profesor data]
        :grupo
        [entidad-datos-grupo data]))


(defn entidad-titulo
  [titulo clase id]
  (let [colisiones (subscribe [:colisiones-de clase id])
        libre      (subscribe [:libre-de clase id])
        htid (keyword (str "entidad-titulo-" clase id))
        ]
    (fn []
      (let [eslibre (or (true? @libre) (nil? @libre))]
      [(utils/elem-clases :span [:tabla-titulo
                                 (if eslibre
                                   :entidad-libre
                                   :entidad-ocupada)])
       {:id htid}
       [:span
        {:on-double-click
         #(dispatch [:recargar-colisiones-y-libres clase id])}
        (str (if (and eslibre (= clase :espacio)) "* ") titulo " ")]
       (if (> @colisiones 0)
         [:span.colisiones-locales
          {:title "Colisiones."}
          (str "(" @colisiones ") " )])]))))


(defn entidad-panel [clase id titulo]
  (let [data (subscribe [clase id])
        libre (subscribe [:libre-de clase id])
        fprofes (subscribe [:filtros-profes])]
    (fn []
      (let [hay-filtros (some (fn [[k v]] (true? v)) @fprofes)]
        [:div
         {:id (str clase id) :data-id id}
         ^{:key (str "entidad-titulo-" clase id) }
         [entidad-titulo titulo clase id]
         [:span.tabla_toggle [toggle {} clase id :_tabla]]
         " "
         [:span.tabla_toggle [toggle {} clase id :_datos]]
         [entidad-datos {} data clase id]
         [:div [entidad-data-panel {} clase id]]
         [:div [entidad-tabla      {} clase id]]
         [:br]]))))

;;;;;;;;;;;;;;;;; GRUPO

(defn carrera [ops c]
  (let [grs (subscribe [:gxcarrera (:id c)])]
    (fn[]

      (if (> (count @grs) 0)
        (into [:div
               ops
               [:h2 (:nombre c)]]

              (for [gr @grs]
                ^{:key (str "cgrupo" (:id gr))}
                [entidad-panel :grupo (:id gr) (:codigo gr)]))
        nil))))

(defn carreras []
  (let [cas (subscribe [:carreras])]
    (fn []
      [:div (for [ca @cas]
              [carrera {:key (:id ca)} ca])])))





;;;;;;;;;;;;;;;;;; Profesores
(defn lista-profesores
  []
  (let [profesores (subscribe [:profesores])
        ]
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
  "Crea la lista de espacios por el tipo de espacio."
  [tid]
  (let [espacios (subscribe [:espaciosx tid])]
    (fn []
      (into
        [:div]
        (for [e @espacios]
           ^{:key (str "espacio" (:id e))}
           [entidad-panel
            :espacio
            (:id e)
            (:nombre e)])))))

(defn toggle-tespacio
  [tid thid hide]
  [:span
   [mtp :toggle [rui/icon-button
                 {:on-click #(dispatch [:toggle-tespacio tid (not hide)])
                  :title (if hide "Mostrar" "Ocultar")}
                 (if hide
                   (ic/content-add)
                   (ic/content-remove))]]])

(defn lista-tespacios
  []
  (let [tespacios (subscribe [:tespacios])]
    (fn []
      (into
        [:div]
        (for [tespacio @tespacios]
          (if (> (:espacios tespacio) 0)
            (let [thid (str "tespacio" (:id tespacio))
                  hide (or (:_hide tespacio) false)
                  colisiones (subscribe
                               [:colisiones-tespacio (:id tespacio)])]
              ^{:key thid}
              [:div.tespacio
               {:id thid}
               [:h2
                [toggle-tespacio (:id tespacio) thid hide]
                (:nombre tespacio) " "
                (if (> @colisiones 0)
                  [:span.colisiones-locales
                   {:title "Colisiones."}
                   (str "(" @colisiones ") " )])]
               [(utils/elem-clases :div [(utils/visible (not hide))])
                [lista-espacios (:id tespacio)]]])))))))

;;;;;;;;;;;;;;;;;; MAIN

(defn main-panel-name
  [ui]
  (let [colis (subscribe [:colisiones-clase (:entidad @ui)])]
    (fn []
      [:h2
       (case (:entidad @ui)
         :profesor "Profesores"
         :grupo "Grupos"
         :espacio "Espacios")
       [:span.colisiones-globales
        {:title "Colisiones en este tipo de entidades."}
        (if (some? @colis)
          (if (> @colis 0)
            (str " ("  @colis ")"))
          " Calculando ...")]])))

(defn profesor-nombre
  [p]
  [:span (:apellidos p) " " (:nombres p) ] )

(defn filtros-grupo
  []
  (let [ui (subscribe [:ui])
        profesores (subscribe [:profesores])
        fprofes (subscribe [:filtros-profes])]
    (fn []
      [:table
       [:thead
        [:tr [:th {:col-span 2} "Profesor"]]]
       (into
         [:tbody]
         (for [p @profesores]
           ^{:key (str "fp" (:id p))}
           (let [pid (:id p)
                 checked (get @fprofes pid)]
             [:tr
              [:td [mtp :blue [rui/checkbox {:checked checked }]]]
              [:td [profesor-nombre p]]])))])))

(defn filtros-espacio
  []
  (let [ui (subscribe [:ui])
        filtrosdh (subscribe [:filtros-dh])
        filtrando (subscribe [:filtrando-dh])
        dias (h/filtros-dh-dias)
        horas (h/filtros-dh-horas)
        lce (subscribe [:libres-cantidad-entidad (:entidad @ui)])
        ndias ["Lunes" "Martes" "Miercoles" "Jueves" "Viernes"]
        ]
    (fn []
      (let [
            ]
        [:table
         [:thead
          [:tr [:td.centrado {:col-span (inc (count dias))}
                "Espacios Libres " [:strong @lce]]]
          (into [:tr [:th]]
                (map (fn thfd
                       [ndia dia]
                       [:th {:on-click #(dispatch [:toggle-dia dia])} ndia])
                     ndias
                     (range 0 (count ndias))))]
         (into
           [:tbody]
           (for [hora horas]
             ^{:key (str "fdh" hora)}
             (into
               [:tr [:th {:on-click #(dispatch [:toggle-hora hora])} hora]]
               (for [dia dias]
                 ^{:key (str "fdd" dia)}
                 (let [dh (h/diahora2k dia hora)
                       nval (get (:new @filtrosdh) dh)
                       oval (get (:old @filtrosdh) dh)
                       clase (if (not= nval oval) :fd-new :fd-old)
                       ]
                   [:td.centrado
                    [mtp clase [rui/checkbox {:checked nval
                                              :onCheck #(dispatch [:toggle-dh dh])
                                              } ]]])))))
         [:tfoot
          [:tr [:td.centrado {:col-span (inc (count dias))}
                [mtp :blue [:div
                            [rui/flat-button
                              {:disabled (or @filtrando
                                             (= (:new @filtrosdh)
                                                (:old @filtrosdh)))
                               :on-click #(dispatch [:aplicar-filtros-dh])}
                              "Filtrar"]
                            (if @filtrando
                              [rui/circular-progress])]]]]]]))))

(defn cambios-drawer
  []
  (let [ui (subscribe [:ui])
        crs (subscribe [:cambios-recibidos-orden])]
    (fn []
      [rui/mui-theme-provider
       {:mui-theme
        (ui/get-mui-theme
          {:palette {:text-color (ui/color "darkBlack")}})}
       [rui/drawer
        {:open (:cambios-drawer @ui) :openSecondary true :width 350}
        [:div.fdrawer
         [:h3 "Cambios recibidos"
          [:a {:href "#"
               :title "Cerrar"
               :on-click #(dispatch [:toggle-cambios-drawer])}
           "-"]]
         (into [:div.cambios]
               (for [cr @crs]
                 ^{:key (str "crdrawer-cb" (:enviado_el cr))}
                 (let [re (js/Date. (js/Date.parse
                                     (:enviado_el cr))) ]
                   [:div.cambio
                    [:span.lista "- "]
                    [:span.realizado_el 
                     (str (.getFullYear re) "-" (inc (.getMonth re))
                         "-" (.getUTCDate re) " "
                         (.getHours re) ":" (.getMinutes re) ":"
                         (.getSeconds re)) ]
                    " "
                    [:span.usuario (:usuario cr)]
                    ": " 
                    [:span.movimientos (:cantidad cr)] ]))) ]]])))

(defn fdrawer
  []
  (let [ui (subscribe [:ui])]
    (fn []
      [rui/mui-theme-provider
       {:mui-theme
        (ui/get-mui-theme
            {:palette {:text-color (ui/color "darkBlack")}})}
       [rui/drawer
        {:open (:fdrawer @ui) :openSecondary true :width 360}
        ; Aqui van los filtros!!
        [:div.fdrawer
         [:h3 "Filtros " [:a {:href "#"
                              :title "Cerrar"
                             :on-click #(dispatch [:toggle-fdrawer])}
                         "-"]]
         (case (:entidad @ui)
           :profesor "No hay filtros disponibles"
           :grupo "No hay filtros disponibles"
           :espacio [filtros-espacio]) ]]])))

(defn dialogo-espacioa
  [ui]
  (let [espacios (subscribe [:espacios])
        ddata (subscribe [:dialogo-espacioa])]
    (fn []
      [mtp :blue [rui/dialog
                  {:title "Selección de espacio alterno"
                   :actions
                   #js[(reagent/as-element
                         [rui/flat-button
                        {:label "Guardar"
                         :on-click #(dispatch [:guardar-espacioa])}])
                       (reagent/as-element
                       [rui/flat-button
                        {:label "Cancelar"
                         :on-click #(dispatch [:cancelar-espacioa])}])]
                   :open (:open @ddata)
                   :modal true}
                  [:div
                   "Selecciona el espacio alterno para esta clase:"
                   (into
                     [:select
                      {:value (or (:espacioa_id @ddata) 0)
                       :on-change
                       #(dispatch [:cambiar-espacioa
                                   (js/parseInt (-> % (.-target) (.-value)))])}
                      [:option {:value 0 } "-- Nada --"]]
                     (for [e @espacios]
                       ^{:key (str "fdedpa" (:id e) "-")}
                       [:option {:value (:id e) }
                                (str (:nombre  e) " - C: " (:capacidad e))]))
                   ]]])))


(defn main-panel []
  (let [ui    (subscribe [:ui])]
    (fn []
      (if (empty? @ui)
        [:h2 "Waiting"]
        [:div.principal

         [:div.cabecera
          [:div
           [:div.titulo (:hnombre @ui)]
           [main-panel-name ui]]]

         [:div.contenido
          [:br]
          (case (:entidad @ui)
            :grupo
            [:div.visible
             [lista-grupos]]
            :profesor
            [:div.visible
             [lista-profesores]]
            :espacio
            [:div.visible
             [lista-tespacios]
             #_[lista-espacios]]
            :else
            (throw (js/Error.
                     (str "Entidad " (:entidad @ui) " no encontrada"))))]
         [edition-panel]
         [fdrawer]
         [cambios-drawer]
         [dialogo-espacioa]
         #_[:pre (str @(subscribe [:db]))] ]))))

