(ns app.temp-converter
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

(def default-state {:val "" :err ""})

(defn sci-format
"Convert to the scientific notation if the number is >= 10^12"
  [number]
  (if (>= number 1e12)
    (.toExponential number 6)
    number))

(defn c-to-f
"Contvert celsius to fahrenheit"
  [temp]
  (sci-format (Math/ceil (+ 32 (* temp (/ 9 5))))))
(defn f-to-c
"Contvert fahrenheit to celsius "
  [temp]
  (sci-format(Math/floor  (* (- temp 32) (/ 5 9)))))

(defn validNumber?
"Check that input string is a valid Number"
  [string]
  (not (js/Number.isNaN (js/Number string))))

(defn freeze 
"Set the :err in a target state to 'freeze'"
  [state key]
  (swap! state assoc-in [key :err] "freeze"))

(defn on-change
  "Mutate the state atom of the temperature converter according to passed key"
  [e state key]
  (let [new-val (-> e .-target .-value)
        other-key (case key
                    :cel :fah
                    :fah :cel)
        convert-func (case other-key
                       :fah c-to-f
                       :cel f-to-c)]
    (if (=  new-val "")
      (do (swap! state assoc key default-state)
          (freeze state other-key))
      (if (validNumber? new-val)
      (do 
        (swap! state assoc key {:val new-val :err ""})
        (swap! state assoc other-key {:val (convert-func new-val) :err ""}))
        (do
          (freeze state other-key)
          (swap! state assoc key {:val new-val :err "invalid"}))))))

(defn converter []
  (let [state (r/atom {:cel default-state :fah default-state})]
    (fn []
      [wrapper {:title "Temperature Converter" :class "converter"}
       [:div.row
        [:p "Celsius:"]
        [:input.field.celsius
         {:class (str (:err (:cel @state)))
          :data-testid "celsius"
          :type "text"
          :value (:val (:cel @state))
          :on-change #(on-change % state :cel)}]]
       [:div.row
        [:p "Fahrenheit:"]
        [:input.field.fahrenheit
         {:class (str (:err (:fah @state)))
          :type "text"
          :value (:val (:fah @state))
          :on-change #(on-change % state :fah)}]]])))