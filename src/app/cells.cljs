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
  "Focus on a neighboring cell"
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
  [ev active cell]
  (let [key (.-key ev)
        target (.-target ev)]
    (when (not= key "Tab") (.preventDefault ev))
    (case key
      "Enter" (reset! active true)
      " " (reset! active true)
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
      "Delete" (update-cell! cell "")
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
                   :on-key-down #(handle-key-down-cell % active cell)
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

(defonce help-svg
  [:svg {:xmlns "http://www.w3.org/2000/svg" :viewBox "0 0 512 512"}
   [:circle {:cx "256" :cy "162.84" :r "27"}]
   [:path {:d "M256 0C114.497 0 0 114.507 0 256c0 141.503 114.507 256 256 256 141.503 0 256-114.507 256-256C512 114.497 397.492 0 256 0zm0 472c-119.393 0-216-96.615-216-216 0-119.393 96.615-216 216-216 119.393 0 216 96.615 216 216 0 119.393-96.616 216-216 216z"}]
   [:path {:d "M256 214.33c-11.046 0-20 8.954-20 20v128.793c0 11.046 8.954 20 20 20s20-8.955 20-20.001V234.33c0-11.046-8.954-20-20-20z"}]])

(defn help []
  (let [open (r/atom true)]
    (fn []
      [:div.help 
       [:button.icon {:tab-index 0
                   :on-click #(swap! open not)}
        help-svg]
       [:div.data {:class (when @open "open")}
        [:p "Edit: "
         [:ol
          [:li "Enter: to enter or exit edit mode"]
          [:li "Space / type any character / Double click : to start editing a cell"]
          [:li "Delete: empty a cell"]
        ]]
        [:p "Navigation: "
         [:ol
          [:li "Arrow keys: move to neighboring cells"]
          [:li "Home / End: move to the first or last cell in a row"]
          [:li "Alt+Home / Alt+End: move to the first or last cell in a column"]
          [:li "Ctrl+Home / Ctrl+End: move to the first or last cell in the sheet"]]]]])))

(defn cells []
  (fn []
    [wrapper {:title "Cells"}
     [help]
     [:table
      [:tbody
       [:tr [:th ""]
        (for [l COLUMNS] [:th {:key l} l])]
       (for [n (range 1 100)] ^{:key n} [row n])]]]))