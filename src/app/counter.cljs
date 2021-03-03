(ns app.counter)

(defn counter [click-count]
  [:div.counter
   [:p.count @click-count]
   [:input {:type "button" :value "Click me!"
            :on-click #(swap! click-count inc)}]])