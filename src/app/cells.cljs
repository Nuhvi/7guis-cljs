(ns app.cells
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

;; ===============
;; Sheet utilities
;; ===============

(defonce COLUMNS (map char (range 65 91)))

(defonce INITIAL_CELL {:form nil
                       :value nil})

(defn- generate-cell-id
  "Returns a cell identifier from its column and row"
  [col row]
  (str col row))

(defn- generate-row
  "Generates a row of cell atoms"
  [row-no]
  (->> COLUMNS
       (map #(list (generate-cell-id % row-no) (r/atom INITIAL_CELL)))))

(defn- generate-sheet
  "Generates an atom for each cell in the sheet"
  []
  (->> (range 1 100)
       (map generate-row)
       (flatten)
       (apply assoc {})))

(defonce sheet (generate-sheet))

;; ====================
;; Navigation functions
;; ====================

(defn move-focus
  "Focus on a neighbouring cell"
  [target direction]
  (let [row (.-parentElement target)
        rows (.. row -parentElement -children)
        siblings (.-children row)
        pos (.indexOf (js/Array.from siblings) target)]
    (->> (case direction
           :left (.-previousSibling target)
           :right (or (.-nextSibling target) target)  
           :up (nth (js/Array.from (.. row -previousSibling -children)) pos)
           :down (nth (js/Array.from (.. row -nextSibling -children)) pos)
           :first (second (.-children (second rows)))
           :last (last (.-children (last rows)))
           :first-in-row (second siblings)
           :last-in-row (last siblings)
           :first-in-col (nth (js/Array.from (.-children (second rows))) pos)
           :last-in-col (nth (js/Array.from (.-children (last rows))) pos)
           false)
         (.focus))))

;; =====================
;; Cell update functions
;; =====================

(defn update-cell!
  "Updates a cell's form"
  [cell form]
  (swap! cell assoc :form form))

(defn handle-key-down-input
  "Handle key press event in a cell"
  [ev cell]
  (.stopPropagation ev)
  (case (.-key ev)
    "Enter" (move-focus (.. ev -target -parentElement) :down)
    (update-cell! cell (.. ev -target -value))))

(defn- handle-key-down-cell
  "Start editing a cell on Enter, or navigate for arrow keys"
  [ev active]
  (.preventDefault ev)
  (.stopPropagation ev)
  (let [key (.-key ev)
        target (.-target ev)]
    (case key
      "Enter" (reset! active true)
      "ArrowRight" (move-focus target :right)
      "ArrowLeft" (move-focus target :left)
      "ArrowUp" (move-focus target :up)
      "ArrowDown" (move-focus target :down)
      "Home" (cond
               (.-ctrlKey ev) (move-focus target :first)
               (.-altKey ev) (move-focus target :first-in-col)
               :else (move-focus target :first-in-row))
      "End" (cond
               (.-ctrlKey ev) (move-focus target :last)
               (.-altKey ev) (move-focus target :last-in-col)
               :else (move-focus target :last-in-row))
      ;; Enter edit mode if any single character was down
      (when (= 1 (count key)) (reset! active true)))))

(defn- should-blur
  "Check that the new focus target is not a child of the event currentTarget"
  [ev]
  (not (.contains (.-currentTarget ev) (.-relatedTarget ev))))

;; ==========
;; Components
;; ==========

(defn cell
  "Renders a cell given its atom"
  [col row]
  (let [active (r/atom false)
        cell-id (generate-cell-id col row)
        cell (get sheet cell-id)]
    (fn []
      (let [form (:form @cell)]
        [:td.cell {:tab-index (if @active -1 0)
                   :on-key-down #(handle-key-down-cell % active)
                   :on-blur #(when (should-blur %) (reset! active false))
                   :on-double-click #(reset! active true)}
         (if @active
           [:input {:auto-focus true
                    :on-focus #(set! (.. % -target -value) form)
                    :on-change #(update-cell! cell (.. % -target -value))
                    :on-key-down #(handle-key-down-input % cell)}]
           [:div.value form])]))))

(defn row
  "Draws a table row"
  [row-no]
  (fn []
    [:tr
     [:td.row-no row-no]
     (doall
      (for [col COLUMNS] ^{:key col} [cell col row-no]))]))

(defn cells []
  (fn []
    [wrapper {:title "Cells"}
     [:table
      [:tbody
       [:tr [:th ""]
        (for [l COLUMNS] [:th {:key l} l])]
       (for [n (range 1 100)] ^{:key n} [row n])]]]))