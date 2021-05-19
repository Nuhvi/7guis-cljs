(ns app.cells
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [cljs.reader :as reader]
            [app.wrapper :refer [wrapper]]))

;; ===============
;; Sheet utilities
;; ===============

(defonce COLUMNS (map char (range 65 91)))

(defonce INITIAL_CELL {:formula nil
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

;; ===================
;; Formula evaluations
;; ===================

(defonce OPERATORS {:sum +
                    :sub -
                    :div /})

(defn- resolve-symbol
  "Find a predefined operator by its symbol"
  [symbol]
  (get OPERATORS (->> symbol str str/lower-case keyword)))

(def formula-matcher #"([a-zA-Z]+)\s*\((.*)?\)")

(defn- has-formula?
  "Check if there is an excel like formula in a string"
  [string]
  (re-find formula-matcher string))

(defn- lispify
  "Format a formula to a clojure form notation"
  [string]
  (if (has-formula? string)
    (lispify (str/replace string formula-matcher "($1, $2)"))
    string))

(defn- eval-form
  "Evaluate form recursively after resolving operators"
  [form]
  (let [resolved (map #(cond (list? %) (eval-form %)
                             (symbol? %) (resolve-symbol %)
                             :else %)
                      form)
        operator (first resolved)
        args (rest resolved)]
    (try
      (apply operator args)
      (catch :default _ :error))))

(defn- eval-str
  "Evaluate a formula string"
  [string]
  (if (has-formula? string)
    (->> string
         (lispify)
         (reader/read-string)
         (eval-form))
    string))

(defn- set-value!
  "Evaluate the formula and update the value in a cell"
  [cell]
  (let [formula (:formula @cell)]
    (swap! cell assoc :value (if (= "=" (first formula))
                               (eval-str (subs formula 1))
                               formula))))

;; ==============
;; Event handlers
;; ==============
                               
(defn update-formula!
  "Updates a cell's formula"
  [cell formula]
  (swap! cell assoc :formula formula))

(defn handle-key-down-input!
  "Handle key press event in a cell"
  [ev cell]
  (.stopPropagation ev)
  (let [key (.-key ev)]
    (if (or (= key "Enter") (= key "Escape"))
      (move-focus (.. ev -target -parentElement) :down)
      (update-formula! cell (.. ev -target -value)))))

(defn- handle-key-down-cell!
  "Start editing a cell on Enter, or navigate for arrow keys"
  [ev edit-mode? cell]
  (let [key (.-key ev)
        target (.-target ev)]
    (when (not= key "Tab") (.preventDefault ev))
    (case key
      "Enter" (reset! edit-mode? true)
      " " (reset! edit-mode? true)
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
      "Delete" (update-formula! cell "")
      ;; Enter edit mode if any single character was down
      (when (= 1 (count key))
        (reset! edit-mode? true)
        (update-formula! cell key)))))

(defn- should-blur
  "Check that the new focus target is not a child of the event currentTarget"
  [ev]
  (not (.contains (.-currentTarget ev) (.-relatedTarget ev))))

(defn- handle-blur-cell!
  [ev cell edit-mode?]
  (when (should-blur ev)
    (reset! edit-mode? false)
    (set-value! cell)))

;; ==========
;; Components
;; ==========

(defn cell
  "Renders a cell given its atom"
  [col row]
  (let [edit-mode? (r/atom false)
        cell-id (generate-cell-id col row)
        cell (get sheet cell-id)]
    (fn []
      (let [c @cell
            formula (:formula c)
            value (:value c)]
        [:td.cell {:class (when (= value :error) "invalid")
                   :tab-index (if @edit-mode? -1 0)
                   :on-key-down #(handle-key-down-cell! % edit-mode? cell)
                   :on-blur #(handle-blur-cell! % cell edit-mode?)
                   :on-double-click #(reset! edit-mode? true)}
         (if @edit-mode?
           [:input {:auto-focus true
                    :on-focus #(set! (.. % -target -value) formula)
                    :on-change #(update-formula! cell (.. % -target -value))
                    :on-key-down #(handle-key-down-input! % cell)}]
           [:div.value (if (= value :error) "#ERROR!" value)])]))))

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
        [:div "Edit mode: "
         [:ol
          [:li "Enter: to enter or exit edit mode"]
          [:li "Escape: exit edit mode"]
          [:li "Space / Double click : to start editing a cell"]
          [:li "Type any character: override formula"]
          [:li "Delete: empty a cell"]
        ]]
        [:div "Navigation: "
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