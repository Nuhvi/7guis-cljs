(ns app.counter
  (:require [reagent.core :as r]))

(def click-count (r/atom 0))

(defn counter []
  [:div.counter
   [:p.count @click-count]
   [:input {:type "button" :value "Click me!"
            :on-click #(swap! click-count inc)}]])