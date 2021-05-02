(ns app.circle-drawer
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

(defonce RADIUS 15)

(defn getCoordinates
  "Return x and y coordinates of a click event"
  [e]
  (let [target (.-currentTarget e)
        rect (.getBoundingClientRect target)
        x (- (.-clientX e) (-> rect .-left int))
        y (- (.-clientY e) (-> rect .-top int))]
    (list x y)))

(defn draw-circle!
  "Draw a new circle on canvas"
  [coordinates radius circles]
  (swap! circles conj {:id (random-uuid)
                       :x (first coordinates)
                       :y (last coordinates)
                       :r radius}))

(defn handle-canvas-click!
  "Decide whether to add new circle, or edit existing one"
  [e circles redo-list]
  (draw-circle! (getCoordinates e) RADIUS circles)
  (reset! redo-list nil))

(defn keep-undo-list
  "Keep undo-list of changes to a given state"
  [state undo-list]
  (add-watch state :watcher
             (fn [_ _ old-state _]
               (swap! undo-list conj old-state))))

(defn handle-undo!
  "Revert the state to the last saved state in undo-list"
  [state undo-list redo-list]
  (let [undos @undo-list]
    (when-let [previous-state (first undos)]
      (swap! redo-list conj @state)
      (reset! state previous-state)
      (reset! undo-list (rest undos)))))

(defn handle-redo!
  "Revert the state to the last saved state in redo-list"
  [state redo-list]
  (when-let [previous-state (first @redo-list)]
    (reset! state previous-state)
    (reset! redo-list (rest @redo-list))))

(defn drawer []
  (let [circles (r/atom '())
        undo-list (r/atom nil)
        redo-list (r/atom nil)]
    (keep-undo-list circles undo-list)
    (fn []
      [wrapper {:title "Circle drawer"}
       [:svg.canvas
        {:on-click #(handle-canvas-click! % circles redo-list)}
        (for [c @circles]
          [:circle {:key (:id c)
                    :cx (:x c)
                    :cy (:y c)
                    :r (:r c)}])]
       [:input
        {:type "button"
         :value "Undo"
         :on-click #(handle-undo! circles undo-list redo-list)
         :disabled (zero? (count @undo-list))}]
       [:input
        {:type "button"
         :value "Redo"
         :on-click #(handle-redo! circles redo-list)
         :disabled (zero? (count @redo-list))}]])))