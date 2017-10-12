(ns trihorario.edicion.db
  )


(defn init-filtro-dh
  "Inicializa el filtr dias/hora mejorado."
  []
  (let [dias [0 1 2 3 4 ]
        horas [7 8 9 10 11 12 13 14 15 16 17 18]
        hd2k (fn [d h] (keyword (str "d" d "h" h)))]
  (zipmap
    (mapcat #(map (partial hd2k %) horas) dias)
    (repeat false))))

(defonce db {
         :loaded false  ; Para apoyarme en el hot reloading
         :new-hcs []
         :new-has []
         :ui {
              ;:entidad nil
              ;:usuario nil
              ;:hid nil
              ;:ekey nil
              :cambio-recibido   0  ; 0 si ya no hay cambios. 1 2 3 etc mas.
              ; cantidad de entidades cambiadas recibidas
              :cambios-recibidos-cantidad 0 
              :cambios-drawer false ; dice si se muestra el drawer de cambios
              :fdrawer false ; dice si se muestra el drawer de filtros
              :dialogo-espacioa {:open false
                                 :espacioa_id nil
                                 :hc_id nil}}

         ; Horas ocupadas por una entidad. Para cada espacio ej. es un vector
         ; de :dXhY  en un set, de tal manera que se puedan sacar facilmente
         ; comparaciones para ver si un espacio esta libre o no.
         :ocupadas {:profesor {}  
                    :grupo {}
                    :espacio {}
                    }
         ; Entidades que estan libres en las horas filtradas
         :libres {:profesor {}
                  :grupo {}
                  :espacio {}}
         :colisiones {:profesor {}
                      :grupo {}
                      :espacio {}}
         :filtros { ; para los filtros
                   :filtrando-dh false
                   :dh {:old (init-filtro-dh)
                        :new (init-filtro-dh)}
                   :profes {} ; Para filtrar grupos por profes
                   
                   }
        :cambios-recibidos []
        })

