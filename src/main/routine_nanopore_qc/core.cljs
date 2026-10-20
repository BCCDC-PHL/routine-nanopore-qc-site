(ns routine-nanopore-qc.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.set]
            [reagent.core :as r] 
            [reagent.dom.client :as rdomc]
            [react]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [ag-grid-community :refer [ModuleRegistry AllCommunityModule]]
            [ag-grid-react :refer [AgGridReact]]
            [cljs.pprint :refer [pprint]]))

(defonce db (r/atom {}))

(def app-version "v1.0.0")

(defn mean
  "Calculate the artithmetic mean of a collection of numbers"
  [coll]
  (/ (reduce + coll) (count coll)))


(defn round-number
  "Round a number"
  [f]
  (/ (.round js/Math (* 100 f)) 100))


(defn in? 
  "true if coll contains elem"
  [coll elem]  
  (some #(= elem %) coll))


(defn load-sequencing-runs
  "Load sequencing runs."
  []
  (go
    (let [response (<! (http/get "data/runs.json"))]
      (swap! db assoc-in [:runs] (:body response)))))


(defn load-library-qc
  "Load library qc for a sequencing run."
  [run-id]
  (go
    (let [response (<! (http/get  "data/library-qc/" run-id "_library_qc.json"))]
      (when (= 200 (:status response))
        (swap! db assoc-in [:library-qc run-id] (:body response))))))


(defn load-species-abundance
  "Load species abundance info for a sequencing run."
  [run-id]
  (go
    (let [response (<! (http/get "data/species-abundance/" run-id "_species_abundance.json"))]
      (when (= 200 (:status response))
        (swap! db assoc-in [:species-abundance run-id] (:body response))))))


(defn debug-view []
  (let [current-debug (:debug-view @db)
        toggle-debug #(swap! db assoc :debug-view (not current-debug))]
    [:div
     [:button {:on-click toggle-debug} "Toggle Debug View"]
     [:div.debug {:style {:background-color "#CDCDCD" :display (if (:debug-view @db) "block" "none")}}
      [:pre [:code {:style {:font-family "monospace" }}
             (with-out-str (pprint (select-keys @db [:debug-view :selected-run-id])))]]]]))


(defn header []
  [:header {:style {:display "grid"
                    :grid-template-columns "repeat(2, 1fr)"
                    :align-items "center"
                    :height "48px"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :align-items "center"}}
    [:h1 {:style {:font-family "Arial" :color "#004a87" :margin "0px"}} "Routine Nanopore QC"][:p {:style {:font-family "Arial" :color "grey" :justify-self "start"}} app-version]]
   [:div {:style {:display "grid" :align-self "center" :justify-self "end"}}
    [:img {:src "/images/logo.svg" :height "48px"}]]])


(defn get-selected-rows [e]
  (map #(js->clj (.-data %) :keywordize-keys true)
       (-> e
           .-api
           .getSelectedNodes)))


(defn run-selected
  "Callback for when a run is selected"
  [e]
  (let [previously-selected-run-ids (:selected-run-ids @db)
        currently-selected-run-id (:run_id (first (get-selected-rows e)))]
    (load-library-qc currently-selected-run-id)
    (load-species-abundance currently-selected-run-id)
    (swap! db assoc-in [:selected-run-id] currently-selected-run-id)))


(defn cell-renderer-hyperlink-button [text params]
  (str "<button><a href=\"" (.-value params) "\" style=\"color: inherit; text-decoration: inherit\" target=\"_blank\">" text "</a></button>"))


;; AG-Grid Components
(def ag-grid-react (r/adapt-react-class AgGridReact))

(def nanopore-runs-column-defs
  [{:field "run_id"
    :headerName "Run ID"
    :minWidth 200
    :sort "desc"}])

(defn nanopore-runs-table []
  (let [runs (:runs @db)
        row-data (->> runs
                      identity)]
    [:div {:class "ag-theme-balham"
           :style {}}
     [ag-grid-react
      {:defaultColDef {:sortable  true
                       :resizable true
                       :minWidth  80
                       :maxWidth 200}
       :rowData row-data
       :pagination false
       :theme "legacy"
       :rowSelection {:mode "singleRow"
                      :checkboxes true}
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged run-selected}]]))


(def library-sequence-qc-column-defs
  [{:field "library_id"
    :headerName "Library ID"
    :maxWidth 200}
   {:field "project_id"
    :headerName "Project ID"
    :maxWidth 200}
   {:field "inferred_genus_name"
    :headerName "Inferred Genus"
    :maxWidth 200}
   {:field "inferred_species_name"
    :headerName "Inferred Species"
    :maxWidth 200}
   {:field "inferred_genus_percent"
    :headerName "Genus Reads (%)"
    :maxWidth 160
    :filter "agNumberColumnFilter"
    :type "numericColumn"}
   {:field "inferred_species_percent"
    :headerName "Species Reads (%)"
    :maxWidth 160
    :filter "agNumberColumnFilter"
    :type "numericColumn"}
   {:field "inferred_species_genome_size_mb"
    :headerName "Genome Size (Mb)"
    :maxWidth 140
    :filter "agNumberColumnFilter"
    :type "numericColumn"}
   {:field "num_reads"
    :headerName "Num. Reads"
    :maxWidth 140
    :filter "agNumberColumnFilter"
    :type "numericColumn"}
   {:field "num_bases"
    :headerName "Total Bases (Mb)"
    :maxWidth 140
    :filter "agNumberColumnFilter"
    :type "numericColumn"}
   {:field "read_n50"
    :headerName "Read N50"
    :maxWidth 160
    :filter "agNumberColumnFilter"
    :type "numericColumn"}
   {:field "inferred_genus_estimated_depth"
    :headerName "Est. Genus Coverage"
    :maxWidth 172
    :filter "agNumberColumnFilter"
    :type "numericColumn"}
   {:field "inferred_species_estimated_depth"
    :headerName "Est. Species Coverage"
    :maxWidth 172
    :filter "agNumberColumnFilter"
    :type "numericColumn"}
   ]
  )

(defn library-sequence-qc-table []
  (let [currently-selected-run-id (:selected-run-id @db)
        selected-run-library-qc (get-in @db [:library-qc currently-selected-run-id])
        row-data (->> selected-run-library-qc
                      (map (fn [x] (update x :inferred_genus_percent #(if % (.toFixed % 2) nil))))
                      (map (fn [x] (update x :inferred_species_percent #(if % (.toFixed % 2) nil))))
                      (map (fn [x] (update x :num_bases #(when % (.toFixed (/ % 1000000) 3)))))
                      (map (fn [x] (update x :inferred_species_genome_size_mb #(if % (.toFixed % 1) nil))))
                      (map (fn [x] (update x :inferred_genus_estimated_depth #(if % (.toFixed % 2) nil))))
                      (map (fn [x] (update x :inferred_species_estimated_depth #(if % (.toFixed % 2) nil)))))]
     [:div {:class "ag-theme-balham"
            :style {}}
     [ag-grid-react
      {:defaultColDef {:sortable  true
                       :resizable true
                       :minWidth  80
                       :maxWidth 200}
       :columnDefs library-sequence-qc-column-defs
       :rowData row-data
       :pagination false
       :theme "legacy"
       :enableCellTextSelection true
       :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
       :onSelectionChanged #()}
      ]]))

(def library-species-abundance-column-defs
  [{:field "library_id" 
    :headerName "Library ID" 
    :filter "agTextColumnFilter" 
    :pinned "left"}
   {:field "project_id"
    :headerName "Project ID"
    :maxWidth 200}
   {:headerName "Most Abundant Species"
    :children [{:field "abundance_1_name" :maxWidth 140 :headerName "Species Name"}
               {:field "abundance_1_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :filter "agNumberColumnFilter" :type "numericColumn"}]}
   {:headerName "2nd Most Abundant Species"
    :children [{:field "abundance_2_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}
               {:field "abundance_2_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]}
   {:headerName "3rd Most Abundant Species"
    :children [{:field "abundance_3_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}
               {:field "abundance_3_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]}
   {:headerName "4th Most Abundant Species"
    :children [{:field "abundance_4_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}
               {:field "abundance_4_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]}
   {:headerName "5th Most Abundant Species"
    :children [{:field "abundance_5_name" :maxWidth 140 :headerName "Species Name" :sortable true :resizable true :filter "agTextColumnFilter"}
               {:field "abundance_5_fraction_total_reads" :maxWidth 120 :headerName "Abundance (%)" :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]}
   {:headerName "Unclassified"
    :children [{:field "unclassified_fraction_total_reads" :headerName "Abundance (%)" :maxWidth 200 :sortable true :resizable true :filter "agNumberColumnFilter" :type "numericColumn"}]}])

(defn library-species-abundance-table
  "Species Abundance Table Component"
  []
  (let [currently-selected-run-id (:selected-run-id @db)
        selected-run-species-abundance (get-in @db [:species-abundance currently-selected-run-id])
        row-data (->> selected-run-species-abundance
                      (map (fn [x] (update x :abundance_1_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_2_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_3_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_4_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :abundance_5_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      (map (fn [x] (update x :unclassified_fraction_total_reads #(.toFixed (* 100 %) 2))))
                      )]
     [:div {:class "ag-theme-balham"
            :style {}}
      [ag-grid-react
       {:defaultColDef {:sortable  true
                        :resizable true
                        :minWidth  80
                        :maxWidth 200}
        :columnDefs library-species-abundance-column-defs
        :rowData row-data
        :getRowId (fn [params]
                   (let [^js data (.-data params)]
                     (.-library_id data)))
        :pagination false
        :theme "legacy"
        :enableCellTextSelection true
        :onFirstDataRendered #(-> % .-api .sizeColumnsToFit)
        :onSelectionChanged #()}
       ]]))


(defn app []
  [:div {:style {:display "grid"
                  :grid-template-columns "1fr 4fr"
                  :grid-template-rows "repeat(2, 1fr)"
                  :gap "4px"
                  :height "800px"}}
   [:div {:style {:display "grid"
                  :grid-column "1"
                  :grid-row "1 / 3"}}
    [nanopore-runs-table]]
   [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "1"
                  :gap "4px"}}
    [library-sequence-qc-table]]
   [:div {:style {:display "grid"
                  :grid-column "2"
                  :grid-row "2"}}
    [library-species-abundance-table]]])


;;

(defonce root
  (when (exists? js/document)
    (rdomc/create-root (.getElementById js/document "app"))))

(defn render
  "Render the app into the root app div."
  []
  (rdomc/render root [app]))

(defn ^:dev/after-load re-render
  "Hot-reload hook called by shadow-cljs after code changes.
  Re-renders from root so that new component definitions take effect.
  State is preserved because it lives in `defonce` app state db atom."
  []
  (render))

(defn ^:export init!
  "Exported entry point called by shadow-cljs on page load."
  []
  (.registerModules ModuleRegistry #js [AllCommunityModule])
  (load-sequencing-runs)
  (render))
