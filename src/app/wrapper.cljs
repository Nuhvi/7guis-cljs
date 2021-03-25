(ns app.wrapper
  (:require [reagent.core :as r]
            [clojure.string :as str]))

(defn wrapper [{:keys [title class]}]
  (let [this (r/current-component)]
   [:div.component {:class (str/lower-case (or class title))}
    [:p.title title]
    (into  [:div.content] (r/children this))]))