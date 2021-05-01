(ns app.crud
  (:require [reagent.core :as r]
            [goog.dom :as gdom]
            [app.wrapper :refer [wrapper]]))

;; Simulate backend database with an atom
;; outside of the component.
(defonce db (r/atom []))

(defn db-create!
  "Create an entry in the database"
  [name surname]
  (swap! db conj 
         {:id (str (random-uuid))
          :name name
          :surname surname
          :fullname (str surname ", " name)}))

;; Generate initial data
(defonce initial-data
  (do (db-create! "Hans" "Emil")
      (db-create! "Max" "Mustermann")
      (db-create! "Roman" "Tisch")))

(defn find-index
  "Find the first index where the predicate returns true"
  [pred coll]
  (first (keep-indexed #(if (pred %2) %1 nil) coll)))

(defn db-update!
  "Create an entry in the database"
  [id name surname]
  (swap! db assoc 
         (find-index #(= (:id %) id) @db)
         {:id id
          :name name
          :surname surname
          :fullname (str surname ", " name)}))

(defn db-delete!
  "Delete an entry in the database"
  [id]
  (swap! db (fn [coll] (filterv #(not= (:id %) id) coll))))

(defn listbox
  "Renders the listbox using the filtered data in db"
  [{:keys [list value on-change]}]
  [:div.row [:select.field.full-width
             {:value value
              :size 3
              :on-change on-change}
             (for [entry list]
               [:option
                {:value (:id entry)
                 :key (:id entry)}
                (:fullname entry)])]])

;; State helper functions
(defn invalid-input?
  "Check if the name or surname are empty"
  [state]
  (or (= 0 (count (:name state)))
      (= 0 (count (:surname state)))))

(defn nothing-selected?
  "Checks if the state has an active id"
  [state]
  (= 0 (count (:selected state))))

(defn filter-list
  "Filter a list of entries using filter prefix"
  [list ^string prefix]
  (filter #(re-find (re-pattern prefix) (:fullname %)) list))

(defn handle-create!
  "Update the database and component state on-click Create"
  [state]
  (do (db-create! (:name @state) (:surname @state))
      ;;Autofocus on the newly created entry
      (swap! state assoc :selected (:id (last @db)))))

(defn handle-delete!
  "Update the database and component state on-click Delete"
  [state]
  (do (db-delete! (:selected @state))
      ;;Update the :selected id to the id of first entry
      (swap! state assoc :selected (:id (first @db)))))

(defn crud []
  (let [state (r/atom {:name "" :surname "" :prefix "" :selected ""})]
    (fn []
      [wrapper {:title "CRUD"}
       [:div @state]
       [:div.row
        [:p "Filter prefix"]
        [:input.field
         {:on-change #(swap! state assoc :prefix (.. % -target -value))}]]
       [listbox
        {:list (filter-list @db (:prefix @state))
         :value (:selected @state)
         :on-change #(swap! state assoc :selected (.. % -target -value))}]
       [:div.row
        [:p "Name"]
        [:input.field
         {:on-change #(swap! state assoc :name (.. % -target -value))}]]
       [:div.row
        [:p "Surname"]
        [:input.field
         {:on-change #(swap! state assoc :surname (.. % -target -value))}]]
       [:input
        {:type "button"
         :value "Create"
         :disabled (invalid-input? @state)
         :on-click #(handle-create! state)}]
       [:input
        {:type "button"
         :value "Update"
         :disabled (or (nothing-selected? @state)
                       (invalid-input? @state))
         :on-click #(db-update! (:selected @state) (:name @state) (:surname @state))}]
       [:input
        {:type "button"
         :value "Delete"
         :disabled (nothing-selected? @state)
         :on-click #(handle-delete! state)}]])))