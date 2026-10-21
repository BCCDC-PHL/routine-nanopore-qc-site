(ns routine-nanopore-qc.loaders
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [routine-nanopore-qc.state :refer [db]]))

(defn load-json-rows
  "Fetch `url` and, if the response is a JSON array of objects, store it in
  the db at `path`. Anything else is logged and ignored.

  Checking the status alone isn't enough: a dev server (or a misconfigured
  web server) may answer a request for a missing file with index.html and
  a 200 status."
  [url path]
  (go
    (let [{:keys [status body]} (<! (http/get url))]
      (if (and (= 200 status)
               (sequential? body)
               (every? map? body))
        (swap! db assoc-in path body)
        (js/console.warn "Ignoring unexpected response for" url
                         (clj->js {:status status :body-type (str (type body))}))))))

(defn load-sequencing-runs
  "Load sequencing runs."
  []
  (load-json-rows "data/runs.json" [:runs]))

(defn load-library-qc
  "Load library qc for a sequencing run."
  [run-id]
  (load-json-rows (str "data/library-qc/" run-id "_library_qc.json")
                  [:library-qc run-id]))

(defn load-species-abundance
  "Load species abundance info for a sequencing run."
  [run-id]
  (load-json-rows (str "data/species-abundance/" run-id "_species_abundance.json")
                  [:species-abundance run-id]))
