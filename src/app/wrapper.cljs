(ns app.wrapper
  (:require [reagent.core :as r]
            [clojure.string :as str]))

(defn kebab-case
  "Convert string to a kebab-case string"
  [string]
  (str/replace (str/lower-case string) #"\s" "-"))

(defn snake-case
  "Convert string to a snake_case string"
  [string]
  (str/replace (str/lower-case string) #"\s" "_"))

(def source-code-svg
  [:svg {:xmlns "http://www.w3.org/2000/svg"
         :xmlnsXlink "http://www.w3.org/1999/xlink"
         :version "1.1"
         :viewBox "0 0 1000 1000"}
   [:metadata "Svg Vector Icons : http://www.onlinewebfonts.com/icon"]
   [:g
    [:path
     {:d "M320,339.2l-77.5-80.4L10,500l232.5,241.3l77.5-80.4L165,500L320,339.2z"}]
    [:path
     {:d "M680,660.9l77.5,80.4L990,500L757.5,258.7L680,339.2L835,500L680,660.9z"}]
    [:path
     {:d "M603.2,154l102.2,47.7L405,846l-102.2-47.7L603.2,154z"}]]])

(def BASE_URL "https://github.com/Nazeh/7guis-cljs/tree/master/src/app/")

(defn wrapper [{:keys [title class]}]
  (let [this (r/current-component)]
   [:div.component {:class (str/lower-case (or class (kebab-case title)))}
    [:div.top-row
     [:p.title title]
     [:a.code-url
      {:target "_blank"
       :href (str BASE_URL (snake-case title) ".cljs")}
      source-code-svg]]
    (into  [:div.content] (r/children this))]))