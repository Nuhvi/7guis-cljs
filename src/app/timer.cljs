(ns app.timer
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

(def default-state {:elapsed 0 :duration 15})

(defn percentage 
  "Returns the percentage as string, e.g: '10%'"
  [numerator denominator]
  (-> (/ numerator denominator) (min 1) (* 100) (str "%")))

(defn secondify
  "Returns a string represinting the seconds as a float"
  [seconds]
  (-> seconds
      (/ 1)
      (.toFixed 1)
      (str "s")))

(defn change-duration
  "Chagne the duration in state"
  [state new-value]
  (swap! state  assoc :duration new-value))

(def FRAME_COUNT 60)

(def MILISECONDS_PER_FRAME (/ 1000 FRAME_COUNT))

(defn tick
  "Update the state at each interval and limit elapsed <= duration"
  [state]
  (swap! state assoc :elapsed (min
                               (+ (/ 1 FRAME_COUNT)  (:elapsed @state))
                               (:duration @state))))
(declare state)
(declare timer-interval)

(defn timer []
  (r/with-let [state    (r/atom default-state)
               timer-interval (js/setInterval #(tick state)
                                              MILISECONDS_PER_FRAME)]
    [wrapper {:title "Timer" :class "timer"}
     [:div.row
      [:p "Elapsed time: "]
      [:div.meter
       [:div.meter-slider-container
        [:div.meter-slider
         {:style {:width (percentage (:elapsed @state) (:duration @state))}}]]
       [:input.field.meter-value
        {:value (secondify (:elapsed @state)) :readOnly true :tab-index -1}]]]
     [:div.row
      [:p "Duration"]
      [:div.range
       [:input
        {:type "range" :defaultValue 15 :min 0 :max 30
         :on-change #(change-duration state (-> % .-target .-value js/Number))}]
       [:input.field
        {:value (secondify (:duration @state)) :readOnly true :tab-index -1}]]]
     [:input
      {:type "button"
       :on-click #(swap! state assoc :elapsed 0)
       :value "Reset Timer"}]]
    (finally (js/clearInterval timer-interval))))