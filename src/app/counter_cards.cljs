(ns app.counter-cards
  (:require [reagent.core :as r]
            [devcards.core :as dc :refer [defcard deftest]]
            [cljs.test :include-macros true :refer [is]]
            ["@testing-library/react" :refer [render cleanup fireEvent]]
            [app.counter :refer [counter]]))

(defn testing-container
  "The container that should be used to render testing-library react components.
  We want to provide our own container so that the rendered devcards aren't used."
  []
  (let [app-div (js/document.createElement "div")]
    (.setAttribute app-div "id" "testing-lib")
    (js/document.body.appendChild app-div)))

(defcard
  "This is a live interactive development environment using [Devcards](https://github.com/bhauman/devcards).
   You can use it to design, test, and think about parts of your app in isolation.
   
   The two 'cards' below show the two components in this app.")

(defcard counter-card
  (dc/reagent counter)
  (r/atom 0)
  {:inspect-data false
   :frame true
   :history true})

(defcard
  "You can also add tests here and see their results. 
   Below are some tests using [React Testing Library](https://testing-library.com/docs/react-testing-library/intro).
   
   Tests will be ran outside the browser when you run the test command.")

(deftest click-counter-tests-card
  (let [atom (r/atom 0)
        element (r/as-element [counter atom])
        tr (render element #js {:container (testing-container)})]
    (is (.queryByText tr #"^0$") "Should show the initial value as '0'")
    (.click fireEvent (.queryByText tr #"(?i)click me"))
    (r/flush)
    (is (.queryByText tr #"^1$") "Should show the value as '1' after click")
    (.click fireEvent (.queryByText tr #"(?i)click me"))
    (r/flush)
    (is (.queryByText tr #"^2$") "Should show the value as '2' after two clicks")
    (cleanup)))