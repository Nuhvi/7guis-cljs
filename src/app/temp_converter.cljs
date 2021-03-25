(ns app.temp-converter
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

(defn numeric?
"Check that input string is a valid Number"
  [string]
  (not (js/Number.isNaN (js/Number string))))

(defn sci-format
"Convert to the scientific notation if the number is >= 10^12"
  [number]
  (if (>= number 1e12)
    (.toExponential number 6)
    number))

(defn c-to-f
"Contvert celsius to fahrenheit"
  [c]
  (+ 32 (* c (/ 9 5))))

(defn f-to-c
"Contvert fahrenheit to celsius "
  [f]
  (* (- f 32) (/ 5 9)))

(defn convert
  "Convert temprature according to target unit"
  [unit temp]
  (->
   (case unit
     :fah (c-to-f temp)
     :cel (f-to-c temp))
   Math/round
   sci-format))

(defn disable 
"Set the :err in a target state to 'disable'"
  [state key]
  (swap! state assoc-in [key :err] "disable"))

(def default-state {:val "" :err ""})

(defn on-change
  "Mutate the state atom of the temperature converter according to passed key"
  [e state key]
  (let [new-val (-> e .-target .-value)
        other-key (case key
                    :cel :fah
                    :fah :cel)]
    (if (= new-val "")
      (do (swap! state assoc key default-state)
          (disable state other-key))
      (if (numeric? new-val)
        (do
          (swap! state assoc key {:val new-val :err ""})
          (swap! state assoc other-key
                 {:val (convert other-key new-val) :err ""}))
        (do
          (swap! state assoc key {:val new-val :err "invalid"})
          (disable state other-key))))))

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