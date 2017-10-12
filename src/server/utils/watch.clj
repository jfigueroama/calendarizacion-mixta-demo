(ns server.utils.watch
  ""
  (:require [hawk.core :as hawk]))

(defn watched
  "Return the watched files from the user hashmap."
  [hm]
  (keys hm))

(comment
  (defn watch-file2
    "Observa un archivo y ejecuta un handler que recibe el ctx y el
    evento obtenido (:modify)."
    [f hn]
    (let [watcher (hawk/watch!
                    [{:paths [f]
                      :handler (fn [ctx e]
                                 (if (= (get e :kind) :modify)
                                   (hn ctx e)
                                   ctx))}])]
      (reset! hawk-observados
              (assoc @hawk-observados f watcher)))))

(defn stop-watch
  ([hm f]
   (do
     (hawk/stop! (get hm f))
     (dissoc hm f)))
  ([watchd]
   (hawk/stop! watchd)))

(defn stop-watched
  [hm]
  (do
    (doseq [w (keys hm)]
      (stop-watch hm w))
    {}))

(defn watch-all
  "Observes a file and executes a handler.
  Observa un archivo y ejecuta un handler que recibe el ctx y el
  evento obtenido. Solo se hace una vez."
  [f]
  (let [watcher (hawk/watch!
                  [{:paths [f]
                    :context (constantly 0)
                    :handler (fn wfa-handler [ctx e]
                               (do
                                 (println (str "Evento en " f ":\n")
                                          ctx
                                          e)
                                 (inc ctx)))}])]
    watcher))

(defn watch
  "Observes a file and executes a handler.
  Observa un archivo y ejecuta un handler que recibe el ctx y el
  evento obtenido. Solo se hace una vez."
  ([hm f hn]
   (let [ohn (get hm f)
         watcher (hawk/watch!
                   [{:paths [f]
                     :context (constantly 0)
                     :handler (fn wf-handler [ctx e]
                                (if (= (:kind e) :modify)
                                  (do
                                    (try
                                      (hn ctx e)
                                      (catch Exception e
                                        (println 
                                          (.getMessage e) "\n")))
                                    (inc ctx))
                                  (inc ctx)))}])]
     (assoc
       (if ohn
         (stop-watch hm f)
         hm)
       f watcher)))
  ([f hn]
   (get (watch {} f hn) f)))

(defmacro reload
  "Permite definir un namespace rou para recargar cada vez que el archivo file
  ha cambiado, permitiendo ejecutar una serie de acciones en caso de definirse.

  (def b
    (reload (require '[clojure.string :as st])
            \"t.clj\" (println \"hola\") (println \"adios\")))
  "
  [rou file & actions]
  (let [rrou (seq (conj (vec rou) :reload))
        prrou (list 'println (list 'quote rou))]
    `(do
       ~rrou
       (watch ~file (fn [ctx# e#]
                      ~rrou
                      ~prrou
                      ~@actions)))))



