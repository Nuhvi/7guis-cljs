(ns app.temp-converter-cards
  (:require [reagent.core :as r]
            [devcards.core :as dc :refer [defcard deftest]]
            [cljs.test :include-macros true :refer [is]]
            ["@testing-library/react" :refer [render cleanup fireEvent]]
            [app.temp-converter :refer [converter]]))

(defn testing-container
  "The container that should be used to render testing-library react components.
  We want to provide our own container so that the rendered devcards aren't used."
  []
  (let [app-div (js/document.createElement "div")]
    (.setAttribute app-div "id" "testing-lib")
    (js/document.body.appendChild app-div)))

(defcard converter-card
  (dc/reagent converter)
  {:inspect-data false
   :frame true
   :history true})

(deftest converter-tests-card
  (let [element (r/as-element [converter])
        tr (render element #js {:container (testing-container)})]
    (is (= (count (.queryAllByDisplayValue tr "")) 2)
        "Should start with empty value")
    (.change fireEvent
             (.queryByTestId tr "celsius")
             {:target {:value "23"}})
    (is (= "23" (.-value (.queryByTestId tr "celsius"))) "Should XXXXX")
    (cleanup)))