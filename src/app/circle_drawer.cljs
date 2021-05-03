(ns app.circle-drawer
  (:require [reagent.core :as r]
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

(defn add-circle!
  "Add a circle to circles, reset the redo-stack,
   and store perivous circles in undo-stack"
  [e state]
  (reset! state {:redo-stack nil
                 :undo-stack (conj (:undo-stack @state) (:circles @state))
                 :circles (conj (:circles @state)
                                (circle-geo {:center (evt-center e)}))}))

(defn undo!
  "Revert the state to the last saved change in undo-stack"
  [state]
  (reset! state {:circles (first (:undo-stack @state))
                 :redo-stack (conj (:redo-stack @state)
                                   (:circles @state))
                 :undo-stack (rest (:undo-stack @state))}))

(defn redo!
  "Revert the state to the last saved change in redo-stack"
  [state]
  (reset! state {:circles (first (:redo-stack @state))
                 :undo-stack (conj (:undo-stack @state)
                                   (:circles @state))
                 :redo-stack (rest (:redo-stack @state))}))

(defn drawer []
  (let [state (r/atom {:circles nil
                       :undo-stack nil
                       :redo-stack nil})]
    (add-watch state :watcher
               (fn [_ _ old new] (js/console.log "old state " (clj->js old) ", new state " (clj->js new))))
    (fn []
      [wrapper {:title "Circle drawer"}
       [:svg.canvas
        {:on-click #(add-circle! % state)}
        (for [c (:circles @state)]
          [:circle {:key (:id c)
                    :cx (:x c)
                    :cy (:y c)
                    :r (:r c)}])]
       [:div.buttons
        [:input
         {:type "button"
          :value "Undo"
          :on-click #(undo! state)
          :disabled (zero? (count (:undo-stack @state)))}]
        [:input
         {:type "button"
          :value "Redo"
          :on-click #(redo! state)
          :disabled (zero? (count (:redo-stack @state)))}]]])))