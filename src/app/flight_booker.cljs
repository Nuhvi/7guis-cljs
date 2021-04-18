(ns app.flight-booker
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]
            [cljs-time.core :as t]
            [cljs-time.format :as tf]))

(def date-format "dd/MM/yyyy")
(def initial-date (tf/unparse {:format-str  date-format} (t/now)))

(defn parse-date
  "Calculate Date from string with format dd/MM/yyyy"
  [string]
  (tf/parse (tf/formatter date-format) string))

(def default-state
  {:type "one-way"
   :departure-date initial-date
   :invalid-departure? false
   :return-date initial-date
   :invalid-return? false
   :return-disabled? true})

(defn change-type
  "Change booking type and disable return date accordingly"
  [state type]
  (swap! state assoc
         :type type
         :return-disabled? (= type "one-way")))

(defn invalid-date?
  "Check if string represents a valid js date"
  [string]
  (try (not (parse-date string))
       (catch js/Error _ true)))

(defn change-departure
  "Change departure date and set invalid-departure?"
  [state value]
  (swap! state assoc
         :departure-date value
         :invalid-departure? (invalid-date? value)))

(defn change-return
  "Change departure date and set invalid-departure?"
  [state value]
  (swap! state assoc
         :return-date value
         :invalid-return? (invalid-date? value)))

(defn can-not-book
  "Check if all conditions are valid for booking"
  [state]
  (or (:invalid-departure? state)
      (:invalid-return? state)
      (and (not (:return-disabled? state))
           (t/after? (parse-date (:departure-date state))
                     (parse-date (:return-date state))))))

(defn book [state]
  (js/alert (case (:type state)
              "one-way" (str "You have booked a One-way flight for "
                             (:departure-date state))
              "round" (str "You have booked a Round trip flight from "
                           (:departure-date state) " to "
                           (:return-date state)))))

(defn booker []
  (let [state (r/atom default-state)]
    (fn []
      [wrapper {:title "Flight Booker" :class "booker"}
       [:div.row
        [:p "Type: "]
        [:select.field.type
         {:value (:type @state)
          :on-change #(change-type state (-> % .-target .-value))}
         [:option {:value "one-way"} "One-way"]
         [:option {:value "round"} "Round trip"]]]
       [:div.row {:class (if (:invalid-departure? @state) "invalid" "")}
        [:p "Departure date:"]
        [:input.field
         {:value (:departure-date @state)
          :on-change #(change-departure state (-> % .-target .-value))}]]
       [:div.row {:class [(if (:return-disabled? @state) "disabled" "")
                          (if (:invalid-return? @state) "invalid" "")]}
        [:p "Retrun date:"]
        [:input.field
         {:value (:return-date @state)
          :on-change #(change-return state (-> % .-target .-value))}]]
       [:input
        {:type "button"
         :on-click #(book @state)
         :disabled (can-not-book @state)
         :value "Book"}]])))
