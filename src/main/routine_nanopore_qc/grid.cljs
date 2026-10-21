(ns routine-nanopore-qc.grid
  (:require [reagent.core :as r]
            [ag-grid-react :refer [AgGridReact]]))

(def ag-grid-react (r/adapt-react-class AgGridReact))

(defn get-selected-rows [^js e]
  (map #(js->clj (.-data %) :keywordize-keys true)
       (-> e
           .-api
           .getSelectedNodes)))
