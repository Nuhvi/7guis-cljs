(ns app.crud
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

;; ============================================
;; Global state to simulate an external backend
;; ============================================

(defonce users (r/atom []))

(defn generate-user
  "Generate a user using name, surname and id,
   or generate random uuid if id is not provided"
  [{:keys [name surname id]}]
  {:id (or id (str (random-uuid)))
   :name name
   :surname surname})

(defn create-user!
  "Create a new user, and return the new state of users"
  [name surname]
  (swap! users conj (generate-user {:name name :surname surname})))

;; Generate initial data
(defonce initial-data
  (do (create-user! "Hans" "Emil")
      (create-user! "Max" "Mustermann")
      (create-user! "Roman" "Tisch")))
initial-data

(defn find-index
  "Find the first index where the predicate returns true"
  [pred coll]
  (first (keep-indexed #(if (pred %2) %1 nil) coll)))

(defn update-user!
  "Update a given user by id, and return the new state of db"
  [id name surname]
  (swap! users assoc
         (find-index #(= (:id %) id) @users)
         (generate-user {:id id :name name :surname surname})))

(defn delete-user!
  "Delete a given user by id, and return the new sate of db"
  [id]
  (swap! users (fn [coll] (filterv #(not= (:id %) id) coll))))

;; ===================
;; Component functions
;; ===================

(defn invalid-input?
  "Check if the name or surname are empty"
  [state]
  (or (zero? (count (:name state)))
      (zero? (count (:surname state)))))

(defn nothing-selected?
  "Checks if the state has an active id"
  [state]
  (zero? (count (:selected state))))

(defn fullname
  "Generate fullname from user's name and surname"
  [user]
  (str (:surname user) ", " (:name user)))

(defn filter-list
  "Filter a list of users using filter prefix"
  [list ^string prefix]
  (filter #(re-find (re-pattern (str "(?i)" prefix)) (fullname %)) list))

(defn handle-create!
  "Create new user and switch focus to it"
  [state]
  (let [old-state @state
        name (:name old-state)
        surname (:surname old-state)]
    (->> (create-user! name surname)
         last
         :id
         (swap! state assoc :selected))))

(defn handle-update!
  "Update the selected user with name and surname"
  [state]
  (let [old-state @state
        id (:selected old-state)
        name (:name old-state)
        surname (:surname old-state)]
    (update-user! id name surname)))

(defn handle-delete!
  ;; TODO make it switch to the closest users in the list instead
  "Delete an user and switch focus to the first user"
  [state]
  (let [new-users (delete-user! (:selected @state))]
    (swap! state assoc :selected (:id (first new-users)))))


(defn listbox
  "Renders the listbox using a given list of users"
  [{:keys [list value on-change]}]
  [:div.row [:select.field.full-width
             {:value value
              :size 3
              :on-change on-change}
             (for [user list]
               [:option
                {:value (:id user) :key (:id user)}
                (fullname user)])]])

(defn crud []
  (let [state (r/atom {:name ""
                       :surname ""
                       :prefix ""
                       :selected (:id (first @users))})]
    (fn []
      [wrapper {:title "CRUD"}
       [:div.row
        [:p "Filter prefix"]
        [:input.field
         {:on-change #(swap! state assoc :prefix (.. % -target -value))}]]
       [listbox
        {:list (filter-list @users (:prefix @state))
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
       [:div.buttons
        [:input
         {:type "button"
          :value "Create"
          :aria-disabled (invalid-input? @state)
          :on-click #(handle-create! state)}]
        [:input
         {:type "button"
          :value "Update"
          :aria-disabled (or (nothing-selected? @state)
                             (invalid-input? @state))
          :on-click #(handle-update! state)}]
        [:input
         {:type "button"
          :value "Delete"
          :aria-disabled (nothing-selected? @state)
          :on-click #(handle-delete! state)}]]])))