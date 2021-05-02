(ns app.timer
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))


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

(defn set-duration!
  "Set the duration in state"
  [state new-value]
  (swap! state assoc :duration new-value))

;; 60 Frames per seconds
(defonce FPS 60)
;; Seconds per frame
(defonce SECONDS_PER_FRAME (/ 1 FPS))
(defonce MILISECONDS_PER_FRAME (/ 1000 FPS))

(defn tick
  "Update the state at each interval and limit elapsed <= duration"
  [state]
  (swap! state assoc :elapsed (min (+ (:elapsed @state) SECONDS_PER_FRAME)
                                   (:duration @state))))
(declare state)
(declare timer-interval)

(defn timer [duration]
  (r/with-let [state  (r/atom {:elapsed 0 :duration (or duration 10)})
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
         :on-change #(set-duration! state (js/Number (.. % -target -value)))}]
       [:input.field
        {:value (secondify (:duration @state)) :readOnly true :tab-index -1}]]]
     [:input
      {:type "button"
       :on-click #(swap! state assoc :elapsed 0)
       :value "Reset Timer"}]]
    (finally (js/clearInterval timer-interval))))