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
  [:main.app
   [counter 0]
   [converter]
   [booker]
   [timer 15]
   [crud {1 {:fst "Hans" :lst "Emil"}}]
   [drawer]])

(defn ^:dev/after-load render
  "Render the toplevel component for this app."
  []
  (r/render [app] (.getElementById js/document "app")))

(defn ^:export main
  "Run application startup logic."
  []
  (render))
