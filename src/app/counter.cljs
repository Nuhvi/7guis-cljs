(ns app.counter)

(defn counter [click-count]
  [:div.counter.component
   [:p.title "Counter"]
   [:div.content
    [:div.count-container
     [:p "Count:"]
     [:p.count @click-count]]
    [:input {:type "button" :value "Increment"
             :on-click #(swap! click-count inc)}]]])