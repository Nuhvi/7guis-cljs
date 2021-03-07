(ns app.temp-converter
  (:require [reagent.core :as r]))

(defn c-to-f [temp] (+ 32 (* temp (/ 9 5))))
(defn f-to-c [temp] (* (- temp 32) (/ 5 9)))

(defn validNumber? [string]
  (re-find #"^-?\d*(\.|\.\d+)?$" string))

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
      (do (swap! state assoc-in [key :val] new-value)
          (swap! state assoc-in [other-key :err] "freeze"))
      (if (validNumber? new-value)
        (reset! state {key {:val new-value :err ""}
                       other-key {:val (convert-func new-value) :err ""}})
        (do
          (swap! state assoc-in [other-key :err] "freeze")
          (swap! state assoc key {:val new-value :err "invalid"}))))))

(defn converter []
(let [state (r/atom {:cel {:val "" :err ""}
                     :fah {:val "" :err ""}})]
  (fn []
    [:div.converter
     [:input.celsius
      {:class (str "celsius " (:err (:cel @state)))
       :data-testid "celsius"
       :type "text"
       :value (:val (:cel @state))
       :on-change #(on-change % state :cel)}]
     [:input
      {:class (str "fahrenheit " (:err (:fah @state)))
       :type "text"
       :value (:val (:fah @state))
       :on-change #(on-change % state :fah)}]])))