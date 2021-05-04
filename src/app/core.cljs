(ns app.core
  "This namespace contains your application and is the entrypoint for 'yarn start'."
  (:require [reagent.core :as r]
            [app.counter :refer [counter]]
            [app.temp-converter :refer [converter]]
            [app.flight-booker :refer [booker]]
            [app.timer :refer [timer]]
            [app.crud :refer [crud]]
            [app.circle-drawer :refer [drawer]]))

(defn app []
  [:main
   [:header
    [:h1 "7GUIs"]
    [:p
     "This is a live version of an implementation ("
     [:a
      {:href "https://github.com/Nazeh/7guis-cljs"
       :target "_blank"}
      "source"]
     ") of "
     [:a
      {:href "https://eugenkiss.github.io/7guis/"}
      "7GUIs"]
      " with "
     [:a
      {:href "https://reagent-project.github.io/"}
      "Reagent"]
     " and "
     [:a
      {:href "https://clojurescript.org/"}
      "CLJS"]]]
   [:section.app
    [counter 0]
    [converter]
    [booker]
    [timer 15]
    [crud {1 {:fst "Hans" :lst "Emil"}}]
    [drawer]]])

(defn ^:dev/after-load render
  "Render the toplevel component for this app."
  []
  (r/render [app] (.getElementById js/document "app")))

(defn ^:export main
  "Run application startup logic."
  []
  (render))
