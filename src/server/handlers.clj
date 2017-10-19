(ns server.handlers
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core :as json]
            [clojure.pprint :refer [pprint]]
            [cognitect.transit :as transit]
            [domain.core :refer :all]
            [domain.horarios :as horarios]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [translator.core :as translator]

            [clojure.java.shell :refer [sh]])

  (:use ring.util.response)
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))



(defn jresponse
  [data & ops]
  (-> (response (json/generate-string
                  data
                  {:pretty true :escape-non-ascii true}))
      (content-type "application/json")
      (header "Access-Control-Allow-Origin" "*")
      (header "Access-Control-Allow-Headers" "Content-Type")
      (charset "UTF-8")))

(defn tresponse
  "Retorna una response con transit."
  [data & ops]
  (let [out (ByteArrayOutputStream.)
        w   (transit/writer out :json)
        _ (transit/write w data)
        ret (.toString out "UTF-8")]
    (.reset out)
    (-> (response ret)
        (content-type "application/transit+json;")
        (header "Access-Control-Allow-Origin" "*")
        (header "Access-Control-Allow-Headers" "Content-Type")
        (charset "UTF-8"))))

(defn root-handler
  [req]
  (str "<h1>Sistema de Construccion de Horarios </h1>"))


(defn ihorarioasignaciones-handler
  "Handler para obtener las ihorarioasignaciones del un horario dado por id."
  [conf dbc req]
  (let [id (-> req :params :id)
        ias (horarios/ihorarioasignaciones dbc id)]
    (tresponse
      (zipmap (map :id ias) ias))))

(defn ihorarioclases-handler
  "Handler para obtener las iasignaciones del un horario dado por id."
  [dbc req]
  (let [id (-> req :params :id)
        ihcs (horarios/ihorarioclases dbc id)]
    (tresponse
      (zipmap (map :id ihcs) ihcs))))


(defn tabla-handler
  "Handler para obtener alguna tabla como catalogo: carrera, instituto, etc.."
  [dbc req]
  (let [tabla (-> req :params :tabla)
        datos (horarios/tabla dbc tabla)]
    (tresponse
      (zipmap (map :id datos) datos))))

(defn horario-datos-handler
  "Handler para obtener alguna tabla como catalogo: carrera, instituto, etc.."
  [dbc req]
  (let [id (-> req :params :id)]
    (tresponse (horarios/horarios-data dbc id))))

(defn jhorario-datos-handler
  "Handler para obtener alguna tabla como catalogo: carrera, instituto, etc.."
  [dbc req]
  (let [id (-> req :params :id)]
    (jresponse (horarios/horarios-data dbc id))))


(defn trihorario-index
  [conf horario entidad id usuario ekey]
  (let [sname (case (:mode conf)
                :prod "app.js"
                :dev  "app-dev.js")
        ename (case entidad
                :profesor "Profesores"
                :grupo "Grupos"
                :espacio "Espacios")]
    (html5 [:head
            [:meta { :http-equiv "content-type"
                    :content "text/html; charset=UTF-8"}]
            [:meta {:charset="UTF-8"}]
            (include-css "/css/horarios.css")
            [:title (str "Collaborative Timetable Editor - " ename)]]
           [:body
            [:div#app {:data-entidad entidad
                       :data-id id
                       :data-nombre (:nombre horario)
                       :data-usuario usuario
                       :data-mode (:mode conf)
                       :data-ekey ekey
                       :data-data-url (str "/transit/" (:id horario))
                       :data-php-base-url (:admin-site-url conf)}

             [:i "Loading ..."]]
               ; App
            [:script {:src (str "/js/trihorario/edicion/" sname)}]
            #_[:script "trihorario.edicion.core.init();"]])))

(defn trihorario-handler
  "MODIFIED FOR DEMO"
  [conf dbc req]
  (let [hs #{5 7}
        horarios {5 {:id 5
                     :nombre "Timetable 5"}
                  7 {:id 7 :nombre "Timetable 7"}}
        id (-> req :params :id (Integer/parseInt))
        entidad (keyword (-> req :params :entidad))
        usuario (-> req :params :usuario)
        ekey (-> req :params :ekey)
        h (get horarios id)]

    (if (hs id)
      (trihorario-index conf h entidad id usuario ekey)
      (html [:body [:div [:h2 "El horario con id " id " no existe"]]]))))




(defn horario-index
  [conf id usuario]
  (let [sname (case (:mode conf)
                :prod "app.js"
                :dev  "app-dev.js")]
    (html5 [:head
            [:meta { :http-equiv "content-type"
                    :content "text/html; charset=UTF-8"}]
            [:meta {:charset="UTF-8"}]
            (include-css "/css/horarios.css")
            [:title "Sistema de Edici&oacute;n de Horarios"]]
           [:body
            [:div#app {:data-id id
                       :data-usuario usuario
                       :data-mode (:mode conf)
                       :data-php-base-url (:php-base-url conf)}]  ; App
            [:script {:src (str "/js/horario/edicion/" sname)}]
            [:script "horario.edicion.core.init();"]])))

(defn horario-handler
  [conf dbc req]
  (let [id (-> req :params :id)
        usuario (-> req :params :usuario)
        h ($ dbc :horario id)]
    (if h
      (horario-index conf id usuario)
      (html [:body [:div [:h2 "El horario con id " id " no existe"]]]))))

(defn devcards-handler
  [conf dbc req]
  (let [id (-> req :params :id)]
    (if-let [h ($ dbc :horario id)]
      (html5 [:head
              [:meta { :http-equiv "content-type"
                      :content "text/html; charset=UTF-8"}]
              [:meta {:charset="UTF-8"}]
              (include-css "/css/horarios.css")
              [:title "Sistema de Edici&oacute;n de Horarios - Devcards"]]
             [:body
              [:div#app {:data-id id
                         :data-mode "devcards"
                         :data-php-base-url (:php-base-url conf)}]
              [:script {:src (str "/js/horario/edicion/app-devcards.js")}]]))))

(defn horario-metabounds-hn
  "Handler para crear los metabounds"
  [conf dbc req]
  (let [id (get-in req [:params :id])
        h ($ dbc :horario id)
        burl (:admin-site-url conf)
        url (str burl "/scheduling/data/" (:multihorario_id h))
        metabounds (translator/crear-metabounds url)]
    (jresponse metabounds)))

(defn horario-llenar-hn
  "Llena un horario nuevo por id y multihorario_id y te devuelve lo que sale
  de tratar de meterlo al sistema en php despues de crear loas horas.

  TAREAS:
  - sacar scheduling data
  - sacar metabounds
  - crear solucion con mh en python
  - crear horario con solucion y metabounds
  - guardar horario en horario dado a traves de llamada a php
  - enviar resultado a cliente.

  NOTA: Esto tarda bastantito eh.
  "
  [conf dbc req]
  (let [id (get-in req [:params :id])
        h ($ dbc :horario id)
        mhid (:multihorario_id h)
        burl (:admin-site-url conf)
        url (str burl "/scheduling/data/" mhid)
        metabounds (translator/crear-metabounds url)
        jmetabounds (json/generate-string
                      metabounds
                      {:pretty true :escape-non-ascii true})
        solver-call (conj (:solver-call conf) :in jmetabounds)
        csol (apply sh solver-call)
        jsol (:out csol)
        sol (vec (json/parse-string (json/parse-string jsol)))
        horario (translator/creador-horario (:metabounds metabounds) sol)
        gurl (str (:admin-site-url conf) "/horario/" id "/fill")]

    (if (empty? (:err csol))
      (translator/guardar-horario gurl id horario)
      (json/generate-string csol {:pretty true}))))


(defn demo-index
  [req]
  (html5
    [:head
     [:meta { :http-equiv "content-type"
             :content "text/html; charset=UTF-8"}]
     [:meta {:charset="UTF-8"}]
     (include-css "/css/horarios.css")
     [:title "Collaborative Timetable Editor"]]
    [:body
     [:div
      [:h2 "Collaborative Timetable Editor"]
      [:p {:style {:text-align "justify"}}
       "This is a demo for my Collaborative Timetable Editor used at
        the " [:i "Universidad Tecnologica de la Mixteca"] "." [:br]
        "Depending of what you want work with, there are three views
         for changing the timetable: Profesor (Professor), Grupo (Group)
        and Espacio (Rooms). All views will be syncronized so any change
        will be visible to all clients that are working with the same
        timetable." [:br]
        "To edit the timetable, one must select some class sessions, move them
         between days or hours, make them longer or shorter, among other
        options, then save the changes or cancel them. The main panel show the
        actions one can apply to the selected sessions."[:br]
        "The UI shows different red counters that represent the number of
         collisions related to the view. I.E. The Group view shows the
         collisions of sessions that exists between the group courses. The main
        counter show the total collisions that exists for the timetable
        that are related to the groups." [:br]
        "The total collisions of the timetable must be calculated by summing up
         the total collitions of groups, the total collitions of professors
        and the total collitions of rooms."]

      [:p {:style {:text-align "justify"}}
       "This demo doesn't need a database. But, their collaborative
        behaviour will work only with tabs that were opened at the
        same time. That because all of the load a static file and
        then the server broadcast any change to the clients.
        When the app is running in production the clients load the
        corresponding data from the database, so they are always
        syncronized."]
      [:p "Enjoy!" [:br] [:i "Jose Figueroa Martinez"]]
      [:h3 "Timetables:"]
      [:ul
       [:li
        [:div
         [:b "Timetable 5 (2017B)"] [:br]
         [:i "Views: "]
         [:a {:href "/trihorario/edicion/profesor/5"} "Profesor"] ", "
         [:a {:href "/trihorario/edicion/grupo/5"} "Grupo"] ", "
         [:a {:href "/trihorario/edicion/espacio/5"} "Espacio"]]]

       [:li
        [:div
         [:b "Timetable 7 (2017B)"] [:br]
         [:i "Views: "]
         [:a {:href "/trihorario/edicion/profesor/7"} "Profesor"] ", "
         [:a {:href "/trihorario/edicion/grupo/7"} "Grupo"] ", "
         [:a {:href "/trihorario/edicion/espacio/7"} "Espacio"]]]]]]))
