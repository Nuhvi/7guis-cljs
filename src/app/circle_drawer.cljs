(ns app.circle-drawer
  (:require [reagent.core :as r]
            [app.utils :refer [find-pos]]
            [app.wrapper :refer [wrapper]]))

(defonce RADIUS 15)

(defn evt-center
  "Return x and y coordinates of a DOM event"
  [e]
  (let [target (.-currentTarget e)
        rect (.getBoundingClientRect target)
        x (- (.-clientX e) (-> rect .-left int))
        y (- (.-clientY e) (-> rect .-top int))]
    (list x y)))

(defn circle-geo
  "Retrun an object representing a circle"
  ([{:keys [center radius id]}]
   {:id (or id (random-uuid))
    :x (first center)
    :y (last center)
    :r (or radius RADIUS)}))

;; ==============
;; State modifers
;; ==============

;; ========
;; Checkers
;; ========

(defn is-active?
  "Check if given circle is the active circle in state"
  [circle state]
  (= (:id circle) (:id (:active-circle state))))

(defn can-undo?
  "Check if the state has more steps to undo"
  [state]
  (seq (:undo-stack state)))

(defn can-redo?
  "Check if the state has more steps to undo"
  [state]
  (seq (:redo-stack state)))

(defn get-distance
  "Get the distance between two points by their coordinates"
  [x1 y1 x2 y2]
  (Math/sqrt (+ (Math/pow (- y2 y1) 2) (Math/pow (- x2 x1) 2))))

(defn closest-circle
  "Return the closest circle under the mouse"
  [x y circles]
  (->
   (reduce
    (fn [prev c]
      (let [curr-distance (get-distance x y (:x c) (:y c))]
        (if (and (< curr-distance (:r c))
                 (< curr-distance (:d prev)))
          {:d curr-distance :c c}
          prev)))
    {:d ##Inf}
    circles)
   :c))

(defn hover-canvas!
  "Set the id of active circle in state"
  [e state]
  (let [center (evt-center e)
        x (first center)
        y (last center)
        s @state
        closest-circle (closest-circle x y (:circles s))]
    (when (and (not (:popup-open? s))
               (not= closest-circle (:active-circle s)))
      (swap! state assoc :active-circle closest-circle))))

(defn add-circle!
  "Add a circle to circles, reset the redo-stack,
   and store perivous circles in undo-stack,
   also save the new circle's id as the active circle"
  [e state]
  (let [s @state
        new-circle (circle-geo {:center (evt-center e)})]
    (swap! state assoc
           :redo-stack nil
           :popup-open? false
           :active-circle new-circle
           :circles (conj (:circles s) new-circle)
           :undo-stack (conj (:undo-stack s) (:circles s)))))

(defn undo!
  "Revert the state to the last saved change in undo-stack"
  [state]
  (let [s @state]
    (when (can-undo? s)
      (swap! state assoc
             :popup-open? false
             :active-circle nil
             :circles (first (:undo-stack s))
             :undo-stack (rest (:undo-stack s))
             :redo-stack (conj (:redo-stack s) (:circles s))))))

(defn redo!
  "Revert the state to the last saved change in redo-stack"
  [state]
  (let [s @state]
    (when (can-redo? s)
      (swap! state assoc
             :popup-open? false
             :active-circle nil
             :circles (first (:redo-stack s))
             :redo-stack (rest (:redo-stack s))
             :undo-stack (conj (:undo-stack s) (:circles s))))))

(defn click-circle!
  "Open a dialog box to edit circle diameter"
  [e state c]
  (.stopPropagation e)
  (swap! state assoc
         :active-circle c
         :popup-open? true
         :slider-open? false))

(defn change-diameter!
  "Update the active circle in the state with given diameter"
  [state diameter]
  (swap! state assoc
         :active-circle (assoc (:active-circle @state) :r (/ diameter 2))))

(defn commit-diameter!
  "Add the new circles with updated diameter to the undo stack"
  [state]
  (let [s @state
        new-circle (:active-circle s)
        circles (:circles s)
        position (find-pos (:id new-circle) circles)]
    (when (not= (:r (circles position)) (:r new-circle))
      (swap! state assoc
             :popup-open? false
             :redo-stack nil
             :circles (assoc circles position new-circle)
             :undo-stack (conj (:undo-stack s) (:circles s))))))

;; =========
;; Component
;; =========

(defn modified-origin
  "Return a translated origin to avoid having the popup out of canvas"
  ;; TODO implement the intersection calculations
  [origin]
  (list (:x origin) (:y origin)))

(defn popup
  [state]
  (let [[x y] (modified-origin (:active-circle @state))]
    (fn []
      [:form.popup {:class (when (:popup-open? @state) "open")
                    :on-blur #(do (commit-diameter! state)
                                  (swap! state assoc :popup-open? false))
                    :style {:left x :top y}}
       (if (:slider-open? @state)
         [:div
          [:p "Adjust Diameter"]
          [:input {:auto-focus true
                   :type "range"
                   :value (* 2 (:r (:active-circle @state)))
                   :on-change #(change-diameter! state (.. % -target -value))}]]
         [:input {:auto-focus true
                  :type "button"
                  :value "Adjust Diameter"
                  :on-click #(swap! state assoc :slider-open? true)}])])))


(defonce INITIAL_STATE {:circles []
                         :undo-stack nil
                         :redo-stack nil
                         :active-circle nil
                         :popup-open? false
                         :slider-open? false})

(defn drawer []
  (let [state (r/atom INITIAL_STATE)]
    (add-watch state :watcher
               (fn [_ _ old new] (js/console.log "old state " (clj->js old) ", new state " (clj->js new))))
    (fn []
      [wrapper {:title "Circle drawer"}
       [:div.canvas
        [:svg {:on-click #(add-circle! % state)
               :on-mouse-move #(hover-canvas! % state)}
         (doall (for [c (:circles @state)]
                  [:circle {:key (:id c)
                            :cx (:x c)
                            :cy (:y c)
                            :r (:r (if (is-active? c @state) (:active-circle @state) c))
                            :class (when (is-active? c @state) "active")
                            :on-click #(click-circle! % state c)}]))]
        (when (:popup-open? @state) [popup state])]
       [:div.buttons
        [:input {:type "button"
                 :value "Undo"
                 :on-click #(undo! state)
                 :aria-disabled (not (can-undo? @state))}]
        [:input {:type "button"
                 :value "Redo"
                 :on-click #(redo! state)
                 :aria-disabled (not (can-redo? @state))}]
        [:input {:type "button"
                 :value "Reset"
                 ;; TODO add can-reset?
                 :on-click #(reset! state INITIAL_STATE)
                 :aria-disabled (zero? (count (:circles @state)))}]]])))