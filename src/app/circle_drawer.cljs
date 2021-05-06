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

(defn add-circle!
  "Add a circle to circles, reset the redo-stack,
   and store perivous circles in undo-stack,
   also save the new circle's id as the active circle"
  [e state]
  (let [s @state
        new-circle (circle-geo {:center (evt-center e)})]
    (swap! state assoc
           :redo-stack nil
           :popup-open false
           :active-circle new-circle
           :circles (conj (:circles s) new-circle)
           :undo-stack (conj (:undo-stack s) (:circles s)))))

(defn undo!
  "Revert the state to the last saved change in undo-stack"
  [state]
  (let [s @state]
    (swap! state assoc
           :popup-open false
           :active-circle nil
           :circles (first (:undo-stack s))
           :undo-stack (rest (:undo-stack s))
           :redo-stack (conj (:redo-stack s) (:circles s)))))

(defn redo!
  "Revert the state to the last saved change in redo-stack"
  [state]
  (let [s @state]
  (swap! state assoc
         :popup-open false
         :active-circle nil
         :circles (first (:redo-stack s))
         :redo-stack (rest (:redo-stack s))
         :undo-stack (conj (:undo-stack s) (:circles s)))))

(defn click-circle!
  "Open a dialog box to edit circle diameter"
  [e state c]
  (.stopPropagation e)
  (swap! state assoc
         :active-circle c
         :popup-open true
         :slider-open false))

(defn hover-circle!
  "Set the id of active circle in state"
  [e state id]
  ;; (swap! state assoc :active id)
  (js/console.log (clj->js (.-target e))
  ))

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
             :popup-open false
             :redo-stack nil
             :circles (assoc circles position new-circle)
             :undo-stack (conj (:undo-stack s) (:circles s))))))

;; ========
;; Checkers
;; ========

(defn is-active?
  "Check if given circle is the active circle in state"
  [circle state]
  (= (:id circle) (:id (:active-circle state))))

;; =========
;; Component
;; =========

(defn drawer []
  (let [state (r/atom {:circles []
                       :undo-stack nil
                       :redo-stack nil
                       :active-circle nil
                       :popup-open false
                       :slider-open false})]
    (add-watch state :watcher
               (fn [_ _ old new] (js/console.log "old state " (clj->js old) ", new state " (clj->js new))))
    (fn []
      [wrapper {:title "Circle drawer"}
       [:div.canvas
        [:svg {:on-click #(add-circle! % state)}
         (doall (for [c (:circles @state)]
                  [:circle {:key (:id c)
                            :cx (:x c)
                            :cy (:y c)
                            :r (:r (if (is-active? c @state) (:active-circle @state) c))
                            :class (when (is-active? c @state) "active")
                            :on-click #(click-circle! % state c)
                            :on-mouse-over #(hover-circle! % state (:id c))}]))]
        [:form.popup {:class (when (:popup-open @state) "open")
                     :on-blur #((commit-diameter! state)
                                (prn "BLUUURRRed"))
                     :style {:top (:y (:active-circle @state))
                             :left (:x (:active-circle @state))}}
         (if (:slider-open @state)
           [:div
            [:p "Adjust Diameter"]
            [:input {:type "range"
                     :value (* 2 (:r (:active-circle @state)))
                     :on-change #(change-diameter! state (.. % -target -value))}]]
           [:input {:type "button"
                    :value "Adjust Diameter"
                    :on-click #(swap! state assoc :slider-open true)}])]]
       [:div.buttons
        [:input {:type "button"
                 :value "Undo"
                 :on-click #(undo! state)
                 :aria-disabled (zero? (count (:undo-stack @state)))}]
        [:input {:type "button"
                 :value "Redo"
                 :on-click #(redo! state)
                 :aria-disabled (zero? (count (:redo-stack @state)))}]]])))