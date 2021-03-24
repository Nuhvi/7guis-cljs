(ns app.wrapper
  (:require [clojure.string :as str]))

(defn wrapper [{:keys [title class]} & children]
  [:div.component {:class (str/lower-case (or class title))}
   [:p.title title]
   [:div.content children]])