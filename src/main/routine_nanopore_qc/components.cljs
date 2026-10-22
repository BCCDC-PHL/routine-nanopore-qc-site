(ns routine-nanopore-qc.components
  (:require [reagent.core :as r]
            [cljs.pprint :refer [pprint]]
            [goog.object :as gobj]
            [routine-nanopore-qc.state :refer [db]]
            [routine-nanopore-qc.loaders :as loaders]
            [routine-nanopore-qc.grid :as grid]))

;; Error Handling

(defn error-boundary
  "Wrap `child` so that an exception thrown while rendering it shows an
  error message in its place instead of unmounting the whole app.
  The error is tied to `reset-key`; when that changes (e.g. a different
  run is selected) the child is rendered again."
  [_reset-key _child]
  (let [error (r/atom nil)
        current-key (atom nil)]
    (r/create-class
     {:display-name "ErrorBoundary"
      :get-derived-state-from-error (fn [e]
                                      (reset! error {:error e :key @current-key})
                                      #js {})
      :component-did-catch (fn [_this e ^js info]
                             (js/console.error "Render error caught by error boundary:" e
                                               (.-componentStack info)))
      :reagent-render (fn [reset-key child]
                        (reset! current-key reset-key)
                        (let [{e :error k :key} @error]
                          (if (and e (= k reset-key))
                            [:div.panel-error
                             [:p "Something went wrong displaying this table."]
                             [:pre (str e)]
                             [:button {:on-click #(reset! error nil)} "Try again"]]
                            child)))})))

;; Header & Debug Components

(defn debug-view []
  (let [current-debug (:debug-view @db)
        toggle-debug #(swap! db assoc :debug-view (not current-debug))]
    [:div
     [:button {:on-click toggle-debug} "Toggle Debug View"]
     [:div.debug {:style {:background-color "#CDCDCD" :display (if (:debug-view @db) "block" "none")}}
      [:pre [:code {:style {:font-family "monospace"}}
             (with-out-str (pprint (select-keys @db [:debug-view :selected-run-id])))]]]]))

(defn header
  "Component for displaying the header."
  [app-version]
  [:header {:style {:display "grid"
                    :grid-template-columns "repeat(2, 1fr)"
                    :align-items "center"
                    :height "48px"}}
   [:div {:style {:display "grid"
                  :grid-template-columns "repeat(2, 1fr)"
                  :align-items "center"}}
    [:h1 {:style {:font-family "Arial" :color "#004a87" :margin "0px"}} "Routine Nanopore QC"] [:p {:style {:font-family "Arial" :color "grey" :justify-self "start"}} app-version]]
   [:div {:style {:display "grid" :align-self "center" :justify-self "end"}}
    [:img {:src "images/logo.svg" :height "48px"}]]])

;; Cell Renderers

(defn cell-renderer-hyperlink-button [text params]
  (str "<button><a href=\"" (.-value params) "\" style=\"color: inherit; text-decoration: inherit\" target=\"_blank\">" text "</a></button>"))

;; Sequencing Runs Table

(defn run-selected
  "Callback for when a run is selected"
  [e]
  (when-let [currently-selected-run-id (:run_id (first (grid/get-selected-rows e)))]
    (loaders/load-library-qc currently-selected-run-id)
    (loaders/load-species-abundance currently-selected-run-id)
    (swap! db assoc-in [:selected-run-id] currently-selected-run-id)))

(defn index-qc-metrics
  "Add `:qc_metrics` to `run`: its `:checked_metrics` as a map keyed by
  metric name, e.g. {\"ReadN50\" {:metric \"ReadN50\" :value 10248 ...}}.
  This lets grid columns refer to a metric with a field path such as
  \"qc_metrics.ReadN50.value\". Runs without QC checks are unchanged."
  [run]
  (if-let [checked-metrics (get-in run [:run_qc_check :checked_metrics])]
    (assoc run :qc_metrics (into {} (map (juxt :metric identity)) checked-metrics))
    run))

(def qc-status-class-rules
  "AG-Grid cellClassRules colouring a QC status cell (PASS / FAIL /
  UNDETERMINED) via the `qc-*` classes in style.css."
  {"qc-pass" (fn [^js params] (= "PASS" (.-value params)))
   "qc-fail" (fn [^js params] (= "FAIL" (.-value params)))
   "qc-undetermined" (fn [^js params] (= "UNDETERMINED" (.-value params)))})

(def run-qc-metrics
  "Run-level QC metrics shown in the runs table, in column order.
  `:metric` is the name used in the run's `:checked_metrics`."
  [{:metric "PercentReadsPassed"
    :header-name "% Passed"
    :description "Percentage of reads that passed basecaller quality filtering"
    ;; Two decimals, so a value just under the threshold (e.g. 69.98)
    ;; doesn't display as if it met it (70.0).
    :format #(.toFixed % 2)
    :unit "%"}
   {:metric "NumReadsPassed"
    :header-name "Num. Passed"
    :description "Number of reads that passed basecaller quality filtering"
    :format #(.toLocaleString %)
    :unit ""}
   {:metric "ReadN50"
    :header-name "Read N50"
    :description "Read length N50 of passed reads"
    :format #(.toLocaleString %)
    :unit " bp"}])

(defn qc-metric-column-def
  "Build a runs table column for one of `run-qc-metrics`. The cell is
  highlighted if the metric failed QC, and its tooltip shows the
  threshold that was applied to that run."
  [{:keys [metric header-name description format unit]}]
  (let [metric-field (fn [^js params field]
                       (gobj/getValueByKeys params "data" "qc_metrics" metric field))]
    {:field (str "qc_metrics." metric ".value")
     :headerName header-name
     :headerTooltip description
     :width 110
     :type "numericColumn"
     :filter "agNumberColumnFilter"
     :valueFormatter (fn [^js params]
                       (if-some [v (.-value params)] (format v) ""))
     :tooltipValueGetter (fn [params]
                           (when-some [threshold (metric-field params "threshold")]
                             (str "Threshold: " (metric-field params "pass_above_or_below")
                                  " " threshold unit)))
     :cellClassRules {"qc-fail" #(= "FAIL" (metric-field % "pass_fail"))}}))

(def nanopore-runs-column-defs
  (into [{:field "run_id"
          :headerName "Run ID"
          :width 300
          :minWidth 200
          :sort "desc"}
         {:field "run_qc_check.overall_qc_pass_fail"
          :headerName "QC Status"
          :headerTooltip "Overall Run QC Status"
          :width 120
          :filter "agTextColumnFilter"
          :cellClassRules qc-status-class-rules}]
        (map qc-metric-column-def)
        run-qc-metrics))

(defn nanopore-runs-table []
  (let [runs (:runs @db)
        row-data (map index-qc-metrics runs)]
    [:div {:class "ag-theme-balham"
           :style {}}
     [grid/ag-grid-react
      {:defaultColDef {:sortable  true
                       :resizable true
                       :minWidth  80}
       :columnDefs nanopore-runs-column-defs
       :rowData row-data
       :getRowId (fn [params]
                   (let [^js data (.-data params)]
                     (.-run_id data)))
       :pagination false
       :theme "legacy"
       :rowSelection {:mode "singleRow"
                      :checkboxes true}
       :selectionColumnDef {:width 40
                            :minWidth 40
                            :maxWidth 40
                            :resizable false
                            :suppressSizeToFit true}
       ;; Size columns to their content rather than to the panel, so that
       ;; widening the panel reveals more columns instead of stretching these.
       :autoSizeStrategy {:type "fitCellContents"}
       :tooltipShowDelay 300
       :enableCellTextSelection true
       :onSelectionChanged run-selected}]]))

;; Library QC Table

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
    :type "numericColumn"}])

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
     [grid/ag-grid-react
      {:defaultColDef {:sortable  true
                       :resizable true
                       :minWidth  80
                       :maxWidth 200}
       :columnDefs library-sequence-qc-column-defs
       :rowData row-data
       :pagination false
       :theme "legacy"
       :enableCellTextSelection true
       :onFirstDataRendered (fn [^js e] (-> e .-api .sizeColumnsToFit))
       :onSelectionChanged #()}]]))

;; Species Abundance Table

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
                      (map (fn [x] (update x :unclassified_fraction_total_reads #(.toFixed (* 100 %) 2)))))]
    [:div {:class "ag-theme-balham"
           :style {}}
     [grid/ag-grid-react
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
       :onFirstDataRendered (fn [^js e] (-> e .-api .sizeColumnsToFit))
       :onSelectionChanged #()}]]))

;; Layout

(defn resizable-panel
  "A panel that the user can widen or narrow by dragging its bottom-right
  corner. Its `width` is the initial width; `min-width` and `max-width`
  bound how far it can be dragged."
  [{:keys [width min-width max-width]} child]
  [:div {:style {:display "grid"
                 :width width
                 :min-width min-width
                 :max-width max-width
                 :overflow "hidden"
                 :resize "horizontal"}}
   child])

(defn layout
  "Page layout: a resizable sidebar on the left, and a main area that
  fills the remaining width with `main-panels` stacked vertically."
  [{:keys [sidebar main-panels]}]
  [:div {:style {:display "grid"
                 ;; `auto` lets the sidebar's own width decide the
                 ;; column's width; the main area takes whatever is left.
                 :grid-template-columns "auto minmax(0, 1fr)"
                 :grid-template-rows (str "repeat(" (count main-panels) ", minmax(0, 1fr))")
                 :gap "4px"
                 :height "800px"}}
   [:div {:style {:display "grid"
                  :grid-column "1"
                  :grid-row "1 / -1"}}
    sidebar]
   (for [[i panel] (map-indexed vector main-panels)]
     ^{:key i}
     [:div {:style {:display "grid"
                    :grid-column "2"
                    :grid-row (str (inc i))}}
      panel])])

(defn nanopore
  "Component for displaying all nanopore sequencing run QC data."
  []
  (let [selected-run-id (:selected-run-id @db)]
    [layout
     {:sidebar [resizable-panel {:width "480px"
                                 :min-width "300px"
                                 :max-width "80vw"}
                [error-boundary nil [nanopore-runs-table]]]
      :main-panels [[error-boundary selected-run-id [library-sequence-qc-table]]
                    [error-boundary selected-run-id [library-species-abundance-table]]]}]))
