(ns app.temp-converter
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

(def defaultState {:val "" :err ""})

(defn c-to-f [temp] (+ 32 (* temp (/ 9 5))))
(defn f-to-c [temp] (* (- temp 32) (/ 5 9)))

(defn validNumber? [string]
  (re-find #"^-?\d*(\.|\.\d+)?$" string))

(defn freeze [state key]
  (swap! state assoc-in [key :err] "freeze"))

(defn on-change
  "Mutate the state atom of the temperature converter according to passed key"
  [e state key]
  (let [new-value (-> e .-target .-value)
        other-key (case key
                    :cel :fah
                    :fah :cel)
        convert-func (case other-key
                       :fah c-to-f
                       :cel f-to-c)]
    (if (= (count new-value) 0)
      (do (swap! state assoc key defaultState)
          (freeze state other-key))
      (if (validNumber? new-value)
        (reset! state {key {:val new-value :err ""}
                       other-key {:val (convert-func new-value) :err ""}})
        (do
          (freeze state other-key)
          (swap! state assoc key {:val new-value :err "invalid"}))))))

(defn converter []
  (let [state (r/atom {:cel defaultState :fah defaultState})]
    (fn []
      [wrapper {:title "Temperature Converter" :class "converter"}
       [:div.row
        [:p "Celsius"]
        [:input.field.celsius
         {:class (str (:err (:cel @state)))
          :data-testid "celsius"
          :type "text"
          :value (:val (:cel @state))
          :on-change #(on-change % state :cel)}]]
       [:div.row
        [:p "Fahrenheit"]
        [:input.field.fahrenheit
         {:class (str (:err (:fah @state)))
          :type "text"
          :value (:val (:fah @state))
          :on-change #(on-change % state :fah)}]]])))