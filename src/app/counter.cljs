(ns app.counter
  (:require [app.wrapper :refer [wrapper]]))

(defn counter [click-count]
  [wrapper {:title "Counter"}
    [:div.row
     [:p "Count:"]
     [:input.field {:value @click-count :readOnly true}]]
    [:input {:type "button" :value "Increment"
             :on-click #(swap! click-count inc)}]])