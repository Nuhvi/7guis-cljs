(ns app.wrapper
  (:require [reagent.core :as r]
            [clojure.string :as str]))

(defn kebab-case
  "Convert string to a kebab-case string for class names"
  [string]
  (str/replace (str/lower-case string) #"\s" "-"))

(defn wrapper [{:keys [title class]}]
  (let [this (r/current-component)]
   [:div.component {:class (str/lower-case (or class (kebab-case title)))}
    [:p.title title]
    (into  [:div.content] (r/children this))]))