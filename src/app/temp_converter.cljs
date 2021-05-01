(ns app.temp-converter
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

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
  "Return the state with the :err set to 'disabled'"
  [state key]
  (assoc (key @state) :err "disabled"))

(def default-state {:val "" :err ""})

(defn valid-number-string?
  "Check that input string is a valid Number"
  [string]
  (not (js/Number.isNaN (js/Number string))))

(defn update-state!
  "Update the state after validating the new-val"
  [state new-val key opposite-key]
  (if (= new-val "")
    (reset! state
            {key default-state
             opposite-key (disable state opposite-key)})
    (if (valid-number-string? new-val)
      (reset! state
              {key {:val new-val :err ""}
               opposite-key {:val (convert opposite-key new-val) :err ""}})
      (reset! state
              {key {:val new-val :err "invalid"}
               opposite-key (disable state opposite-key)}))))

(defn set-celsius! 
  "Set the celsius temprature and update the rest accordingly"
  [state new-val]
  (update-state! state new-val :cel :fah))

(defn set-fahrenheit! 
  "Set the fahrenheit temprature and update the rest accordingly"
  [state new-val]
  (update-state! state new-val :fah :cel))

(defn converter []
  (let [state (r/atom {:cel default-state :fah default-state})]
    (fn []
      [wrapper {:title "Temperature Converter" :class "converter"}
       [:div.row {:class (-> @state :cel :err str)}
        [:p "Celsius:"]
        [:input.field.celsius
         {:value (-> @state :cel :val)
          :on-change #(set-celsius! state (.. % -target -value))}]]
       [:div.row {:class (-> @state :fah :err str)}
        [:p "Fahrenheit:"]
        [:input.field.fahrenheit
         {:value (-> @state :fah :val)
          :on-change #(set-fahrenheit! state (.. % -target -value))}]]])))