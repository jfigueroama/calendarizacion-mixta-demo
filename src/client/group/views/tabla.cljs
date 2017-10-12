(ns client.group.views.tabla
  (:require [reagent.core  :as reagent]
            [re-frame.core :refer [subscribe dispatch]]
            [client.utils :as utils]
            [clojure.string :as string]))

(defn grupo-celda
  [ops {haid :haid hcid :hcid hora :hora}]
  (let [profesores (subscribe [:mprofesor])
        asignaturas (subscribe [:masignatura])
        espacios  (subscribe [:mespacio])
        cha (subscribe [:ha haid])
        chc (subscribe [:hc hcid])
        ]
    (fn []
      (let [{nha :new oha :old} @cha
            {nhc :new ohc :old} @chc
            
            profesor (get @profesores (:profesor_id nha))
            asignatura (get @asignaturas (:asignatura_id nha))
            espaciot (get @espacios (:espaciot_id nha))
            grado (get {1 "Lic." 2 "Mtr." 3 "Dr."} (:grado profesor))
            noderivada  (= (:hora nhc) hora)
            clases-celda (string/join
                           "."
                           (filter some?
                                   [(if noderivada "" "hora-extendida")
                                    (if (true? (:_selected @chc)) "pe-seleccionado")
                                    (if (not= nhc ohc) "pe-cambiado") ]))
            ]
        [(if (:vector ops) :div.multicelda :div)
         [(keyword (str "span" (if (not (empty? clases-celda)) ".") clases-celda))
          {:data-horarioclase_id (:id nhc)
           :data-horarioasignacion_id (:id nha)
           :on-click #(dispatch [:seleccionar (:id nhc)])
           }
          grado (:nombres profesor) (:apellidos profesor)
          [:br]
          (:nombre asignatura)
          [:br]
          (:nombre espaciot)
          [:br] ]]))))

(defn grupo-tabla
  [ops grupo gid]
  (let [tabla (subscribe [:tabla-horario-grupo gid])
        mhas  (subscribe [:mhas])
        mhcs  (subscribe [:mhcs]) ]
    (fn []
      (if (:_tabla @grupo)
        (doall
          [:table
           {:id (str "tgrupo" gid)
            ;:class "table table-bordered table-striped"
            }
           [:thead
            [:tr [:th.hora "HORARIO"] [:th "Lunes"] [:th "Martes"]
             [:th "Miércoles"] [:th "Jueves"] [:th "Viernes"] ]]
           (into 
             [:tbody]
             (for [hora (utils/khoras)]
               (if (some some? (vals (get @tabla hora)))

                 ^{:key hora}
                 (into [:tr [:td.hora (utils/hora2texto hora)]]
                       (for [dia (utils/kdias)]
                         (let [celda (get-in @tabla [hora dia])
                               icelda (utils/celda-data celda @mhas @mhcs)
                               hayc (if (vector? celda)
                                      (utils/hay-colisiones :grupo icelda)
                                      false)
                               td (if hayc :td.concol :td)]
                           ^{:key (str dia hora)}
                           [td
                            (cond
                              (nil? celda)
                              ""
                              (vector? celda)
                              (for [c celda]
                                ^{:key (:hcid c)}
                                [grupo-celda {:vector true} c])
                              (map? celda)
                              [grupo-celda {} celda]
                              true
                              [:span "ERROR"])] )) ))) )])))))




(defn grupo-data
  [ops grupo gid]
  (let [mhas  (subscribe [:mhas])
        mhcs  (subscribe [:mhcs]) ]
    (fn []
      (if (:_data @grupo)
        [:span "datos de las asignaciones"]))))
 

