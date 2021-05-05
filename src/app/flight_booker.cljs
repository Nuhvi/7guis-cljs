(ns app.flight-booker
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]
            [cljs-time.core :as t]
            [cljs-time.format :as tf]))

(defonce date-format "dd/MM/yyyy")
(defonce initial-date (tf/unparse {:format-str  date-format} (t/now)))

(defn parse-date
  "Calculate Date from string with format dd/MM/yyyy"
  [string]
  (tf/parse (tf/formatter date-format) string))

(defn invalid-date?
  "Check if string represents a valid js date"
  [string]
  (try (not (parse-date string))
       (catch js/Error _ true)))

(defn set-type!
  "Set booking type and disable return date accordingly"
  [state type]
  (swap! state assoc
         :type type
         :return-disabled? (= type "one-way")))

(defn set-departure!
  "Set departure date and invalid-departure?"
  [state value]
  (swap! state assoc
         :departure-date value
         :invalid-departure? (invalid-date? value)))

(defn set-return!
  "set return date and invalid-departure?"
  [state value]
  (swap! state assoc
         :return-date value
         :invalid-return? (invalid-date? value)))

(defn can-not-book?
  "Check if all conditions are valid for booking"
  [state]
  (or (:invalid-departure? state)
      (:invalid-return? state)
      (and (not (:return-disabled? state))
           (t/after? (parse-date (:departure-date state))
                     (parse-date (:return-date state))))))

(defn book!
  "Alert user with their booked choice"
  [state]
  (js/alert (case (:type state)
              "one-way" (str "You have booked a One-way flight for "
                             (:departure-date state))
              "round" (str "You have booked a Round trip flight from "
                           (:departure-date state) " to "
                           (:return-date state)))))

(defonce default-state  {:type "one-way"
                         :departure-date initial-date
                         :invalid-departure? false
                         :return-date initial-date
                         :invalid-return? false
                         :return-disabled? true})

(defn booker []
  (let [state (r/atom default-state)]
    (fn []
      [wrapper {:title "Flight Booker"}
       [:div.row
        [:p "Type: "]
        [:select.field.type {:value (:type @state)
                             :on-change #(set-type! state (.. % -target -value))}
         [:option {:value "one-way"} "One-way"]
         [:option {:value "round"} "Round trip"]]]
       [:div.row {:class (when (:invalid-departure? @state) "invalid")}
        [:p "Departure date:"]
        [:input.field {:value (:departure-date @state)
                       :on-change #(set-departure! state (.. % -target -value))}]]
       [:div.row {:class [(when (:return-disabled? @state) "disabled")
                          (when (:invalid-return? @state) "invalid")]}
        [:p "Retrun date:"]
        [:input.field {:value (:return-date @state)
                       :aria-disabled (when (:return-disabled? @state) "disabled")
                       :on-change #(set-return! state (.. % -target -value))}]]
       [:div.buttons
        [:input {:type "button"
                 :on-click #(book! @state)
                 :aria-disabled (can-not-book? @state)
                 :value "Book"}]]])))
