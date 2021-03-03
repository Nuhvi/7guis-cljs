(ns app.temp-converter
  (:require [reagent.core :as r]))

(defn c-to-f [temp] (+ 32 (* temp (/ 9 5))))
(defn f-to-c [temp] (* (- temp 32) (/ 5 9)))

(defn on-change-fahrenheit [e state]
  (let [new-value (-> e .-target .-value)]
    (reset! state {:cel (f-to-c new-value) :fah new-value})))

(defn on-change-celsius [e state]
  (let [new-value (-> e .-target .-value)]
    (reset! state {:cel new-value :fah (c-to-f new-value)})))

(defn converter []
(let [state (r/atom {:cel "", :fah ""})]
  (fn []
    [:div.converter
     [:input.celsius
      {:type "text"
       :value (:cel @state)
       :on-change #(on-change-celsius % state)}]
     [:input.fahrenheit
      {:type "text"
       :value  (:fah @state)
       :on-change #(on-change-fahrenheit % state)}]])))