(ns app.counter
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

(defn counter [initial-count]
  (let [click-count (r/atom (or initial-count 0))]
    (fn []
      [wrapper {:title "Counter"}
       [:div.row
        [:p "Count:"]
        [:input.field {:value @click-count :readOnly true}]]
       [:input {:type "button" :value "Increment"
                :on-click #(swap! click-count inc)}]])))