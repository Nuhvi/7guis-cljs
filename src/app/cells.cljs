(ns app.cells
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [clojure.set :as set]
            [cljs.reader :as reader]
            [app.wrapper :refer [wrapper]]))

;; ================
;; Sheet generation
;; ================

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
       (map #(let [id (generate-cell-id % row-no)]
               (list id (r/atom (merge {:id id} INITIAL_CELL)))))))

(defn- generate-sheet
  "Generates an atom for each cell in the sheet"
  []
  (->> (range 1 100)
       (map generate-row)
       (flatten)
       (apply assoc {})))

(defonce sheet (generate-sheet))

(defn- get-cell
  "Get a cell from the sheet given a case insensitive id"
  [cell-id]
 (get sheet (str/upper-case cell-id)))

;; ==========
;; Error logs
;; ==========

(defn- error
  "Throw an object to be caught and logged at update-cell!"
  [code data]
  (throw {:code code :data data}))

(defonce ERROR_NAMES {"#REF!" "Contains Self Reference"
                      "#NAME?" "Unresolved Reference"})

(defn- log-error
  "Log a formatted warning given an error object"
  [cell-id e]
  (js/console.warn
   (str "7GUIs-Cells Error:\n"
        (or (get ERROR_NAMES (:code e)) "Unknown error")
        " at " cell-id ":\n"
        (or (:data e) e))))

;; ==========
;; References
;; ==========

(defn- is-range-ref? 
  "Check if a ref is a range of references"
  [ref]
  (re-find #"(?i)[A-Z]\d\d?:[A-Z]\d\d?" ref))

(defn- resolve-cell-val
  "Get the value of a cell given its id"
  [cell-id]
  (let [cell (get-cell cell-id)]
    (if cell
      (let [val (:value @cell) num (js/Number val)]
        (if (or (str/blank? val) (js/Number.isNaN num)) "" num))
      (error "#NAME?" cell-id))))

(defn- inc-ref
  "Increment a reference assuming the follow the pattern [A->Z][1->99]"
  [ref]
  (let [col (first ref) row (js/Number (subs ref 1))]
    (if (= row 99)
      (str (nth COLUMNS (inc (.indexOf COLUMNS col))) 1)
      (str col (inc row)))))

(defn- range-references
  "Return a list of references inclusive between two refs given a rage like A34:B22"
  [range]
  (let [[start end] (str/split range ":")]
    (concat (take-while #(not= % end) (iterate inc-ref start)) (list end))))

(defn- resolve-reference
  "Get the value of either single cell or a range of cells by reference"
  [sym]
  (let [ref (str/upper-case (str sym))]
    (if (is-range-ref? ref)
      (map #(resolve-cell-val %) (range-references ref))
      (resolve-cell-val ref))))

;; =========
;; Operators
;; =========

(defonce OPERATORS {:sum (fn [& args] (apply + (flatten args)))
                    :sub (fn [& args] (apply - (flatten args)))
                    :div (fn [& args] (apply / (flatten args)))
                    :mul (fn [& args] (apply * (flatten args)))
                    :mod (fn [& args] (apply mod (flatten args)))
                    :avg (fn [& args] (/ (apply + (flatten args))
                                         (count (flatten args))))
                    :count (fn [& args] (count (flatten args)))})

(defn- resolve-operator*
  "Find a predefined operator by its symbol"
  [symbol]
  (get OPERATORS (->> symbol str str/lower-case keyword)))

(def resolve-operator (memoize resolve-operator*))

;; ===================
;; Formula evaluations
;; ===================

(def formula-matcher #"([a-zA-Z]+)\s*\((.*)?\)")

(defn- has-formula?
  "Check if there is an excel like formula in a string"
  [string]
  (re-find formula-matcher string))

(defn- lispify
  "Format a formula to an S-expression"
  [string]
  (if (has-formula? string)
    (lispify (str/replace string formula-matcher "($1, $2)"))
    string))

(defn- eval-form
  "Evaluate form recursively after resolving symbols"
  [form]
  (let [resolved (map #(cond (list? %) (eval-form %)
                             (symbol? %) (or (resolve-operator %)
                                             (resolve-reference %))
                             :else %)
                      form)
        operator (first resolved)
        args (rest resolved)]
    (apply operator args)))

(defn- eval-str
  "Evaluate a formula string"
  [string]
  (if (has-formula? string)
    (->> string
         (lispify)
         (reader/read-string)
         (eval-form))
    ;; If there is a letter after = it should be a reference
    (if (re-find #"[a-zA-Z]" string)
      (resolve-reference string)
      string)))

(defn- contains-self-reference?
  [cell-id string]
  (re-find (re-pattern (str "(?i)[, (]?" cell-id "[, )]?")) string))

(defn- eval-formula*
  [cell-id formula]
  (if (= "=" (first formula))
    (if (contains-self-reference? cell-id formula)
      (error "#REF!" formula)
      (eval-str (subs formula 1)))
    formula))

(def eval-formula (memoize eval-formula*))

;; ===================
;; Forward propagation
;; ===================

(defn- find-refs
  "Return all single and range references in a formula"
  [formula]
  (if formula
    (re-seq #"(?i)[A-Z]\d\d?(?::[A-Z]\d\d?)?" formula)
    (list)))

(defn- expand-ranges
  "Crawl through a list of refs and expand range refs to a list of refs"
  [refs]
  (map #(if (is-range-ref? %) (range-references %) %) refs))

(defn- formula-refs
  "Return a set of references a formula"
  [formula]
  (->> formula
       (find-refs)
       (expand-ranges)
       (flatten)
       (set)))

(defn- re-calculate
  "Recalculate the cell value whenever a the value of a referenced cell changes"
  [cell-id _ old-val new-val]
  (when (not= (:value old-val) (:value new-val))
    (let [cell (get-cell cell-id)
          c @cell
          formula (:formula c)]
      (try
        (swap! cell assoc
               :formula formula
               :value (eval-formula* cell-id formula)
               :err false)
        (catch :default e
          ((log-error cell-id e)
           (swap! cell assoc
                  :formula formula
                  :err (or (:code e) "#Error!"))))))))

(defn- update-watchers
  "Remove all previous watchers and add new watchers for referenced cells in formula"
  [cell-id old-formula new-formula]
  (let [old-refs (formula-refs old-formula)
        new-refs (formula-refs new-formula)
        remove-refs (set/difference old-refs new-refs)
        add-refs (set/difference new-refs old-refs)]
    (doseq [watched (map #(get-cell %) remove-refs)]
      (when watched (remove-watch watched cell-id)))
    (doseq [to-watch (map #(get-cell %) add-refs)]
      (when to-watch (add-watch to-watch cell-id re-calculate)))))

;; ====================
;; Navigation functions
;; ====================

(defn- move-focus
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

;; ==============
;; Event handlers
;; ==============

(defn- update-cell!
  "Updates a cell's formula and Evalute it to set the value"
  [cell formula]
  (let [c @cell
        cell-id (:id c)
        old-formula (:formula c)]
    (when (not= old-formula formula)
      (try
        (swap! cell assoc
               :formula formula
               :value (eval-formula cell-id formula)
               :err false)
        (update-watchers cell-id old-formula formula)
        (catch :default e
          ((log-error cell-id e)
           (swap! cell assoc
                  :formula formula
                  :err (or (:code e) "#Error!"))))))))

(defn handle-key-down-input!
  "Handle key press event in a cell"
  [ev edit-mode? cell]
  (.stopPropagation ev)
  (let [key (.-key ev)
        parent (.. ev -target -parentElement)]
    (when (contains? #{"Enter" "Tab"} key)
      (update-cell! cell (.. ev -target -value)))
    (when (= key "Escape")
      (reset! edit-mode? false)
      (.focus parent))
    (when (= key "Enter")
      (move-focus parent :down))))

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
      "Delete" (update-cell! cell "")
      ;; Enter edit mode if any single character was down
      (when (= 1 (count key))
        (reset! edit-mode? true)
        ;; A little hacky way to carry the first key to the
        ;; value of the Input element after it is added to the DOM.
        (js/setTimeout
         #(set! (.-value (first (.-children target))) key)
         60)))))

(defn- should-blur
  "Check that the new focus target is not a child of the event currentTarget"
  [ev]
  (not (.contains (.-currentTarget ev) (.-relatedTarget ev))))

(defn- handle-blur-cell
  "Update cell by the input value when and reset edit-mode"
  [ev edit-mode? cell]
  (when (should-blur ev)
    ;; Update cell if the focus was on Input before blur
    (when (not= (.-target ev) (.-currentTarget ev))
      (update-cell! cell (.. ev -target -value)))
    (reset! edit-mode? false)))

;; ==========
;; Components
;; ==========

(defn cell
  "Renders a cell given its atom"
  [col row]
  (let [edit-mode? (r/atom false)
        cell-id (generate-cell-id col row)
        cell (get-cell cell-id)]
    (fn []
      (let [c @cell
            formula (:formula c)
            value (:value c)
            err (:err c)]
        [:td.cell {:class (when err "invalid")
                   :tab-index (if @edit-mode? -1 0)
                   :on-key-down #(handle-key-down-cell! % edit-mode? cell)
                   :on-blur #(handle-blur-cell % edit-mode? cell)
                   :on-double-click #(reset! edit-mode? true)}
         (if @edit-mode?
           [:input {:auto-focus true
                    :on-focus #(set! (.. % -target -value) formula)
                    :on-key-down #(handle-key-down-input! % edit-mode? cell)}]
           [:div.value (if err err (str value))])])))) ;; cast to string in case of NaN

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
  (let [open (r/atom false)]
    (fn []
      [:div.help
       [:button.icon {:tab-index 0
                      :on-click #(swap! open not)}
        help-svg]
       [:div.data {:class (when @open "open")}
        [:div "Navigation: "
         [:ol
          [:li "Tab / Arrow keys: move to neighboring cells."]
          [:li "Home / End: move to the first or last cell in a row."]
          [:li "Alt+Home / Alt+End: move to the first or last cell in a column."]
          [:li "Ctrl+Home / Ctrl+End: move to the first or last cell in the sheet."]]]
        [:div "Edit mode: "
         [:ol
          [:li "Enter: to enter or exit edit mode."]
          [:li "Escape: exit edit mode and discard changes."]
          [:li "Space / Double click : to start editing a cell."]
          [:li "Type any character: override formula."]
          [:li "Delete: empty a cell."]]]
        [:div "References: "
         [:ol
          [:li "You can reference the value of other cell separately '=F1'"]
          [:li "You can reference the value of other cell in a formula '=SUM(1, A1)'"]
          [:li "You can reference a range of cells as a list '=SUM(A1:A5, B1:B5)'"]
          [:li "Cells values propagate to all cells referencing them."]]]
        [:div "Formulas: "
         [:ol
          [:li "Formulas should start with an = sign '=SUM()'"]
          [:li "You can nest formulas inside each others '=SUM(1, 2, div(1, 5))'"]
          [:li "Formulas get converted to variadic clojure forms '=SUM(1,2)'>>'(+ 1 2)'"]
          [:li "Available operations: "
           (map #(str (str/upper-case (name %)) " ") (keys OPERATORS))]]]]])))

(defn cells []
  (fn []
    [wrapper {:title "Cells"}
     [help]
     [:table
      [:tbody
       [:tr [:th ""]
        (for [l COLUMNS] [:th {:key l} l])]
       (for [n (range 1 100)] ^{:key n} [row n])]]]))