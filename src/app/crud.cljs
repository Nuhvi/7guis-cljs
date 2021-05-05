(ns app.crud
  (:require [reagent.core :as r]
            [app.wrapper :refer [wrapper]]))

;; ==========
;; Generators
;; ==========

(defn initial-state
  "Generate the intial state of the crud component,
   given an intial vector of users"
  [users]
  {:name ""
   :surname ""
   :prefix ""
   :users users
   :selected (:id (first users))})

(defn fullname
  "Generate fullname from user's name and surname"
  [user]
  (str (:surname user) ", " (:name user)))

(defn generate-user
  "Generate a user using name, surname and id,
   or generate random uuid if id is not provided"
  [{:keys [name surname id]}]
  {:id (or id (str (random-uuid)))
   :name name
   :surname surname})

;; ========
;; Checkers
;; ========

(defn valid-input?
  "Check if both the name and surname are not empty"
  [state]
  (and (seq (:name state))
       (seq (:surname state))))

(defn has-selected?
  "Checks if the state has a selected id"
  [state]
  (seq (:selected state)))

(defn has-no-prefix?
  "check if the state has no perfix input for filtering"
  [state]
  (not (seq (:prefix state))))

(defn can-create?
  "Check if the state has no prefix,
   but contains valid name and surname"
  [state]
  (and (valid-input? state)
       (has-no-prefix? state)))

(defn can-update?
  "Check if the state has an active selection, 
   no prefix and valid name and surname"
  [state]
  (and (has-selected? state)
       (valid-input? state)
       (has-no-prefix? state)))

(defn can-delete?
  "Check if the state has an active selection,
   and no prefix"
  [state]
  (and (has-selected? state)
       (has-no-prefix? state)))

;; ==============
;; vector helpers
;; ==============

(defn find-pos
  "Find the position of a user in a vector by their uuid"
  [id coll]
  (first (keep-indexed #(when (= (:id %2) id) %1) coll)))

(defn filter-users
  "Filter a list of users using filter prefix"
  [users ^string prefix]
  (filter #(re-find (re-pattern (str "(?i)" prefix)) (fullname %)) users))

(defn vec-remove
  "Remove an item from a vector at a given index"
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn find-nearest
  "Find the nearest item in a collection from a given position"
  [pos coll]
  (if (>= (count coll) (inc pos))
    (coll pos)
    (last coll)))

;; ==============
;; State modifers
;; ==============

(defn set-target-value
  "Set the value of a given key in the state to the event target value"
  [state key event]
  (swap! state assoc key (.. event -target -value)))

(defn handle-create!
  "Create new user and switch selected its uuid"
  [state]
  (let [s @state
        new-user (generate-user {:name (:name s) :surname (:surname s)})]
    (when (can-create? s))
    (swap! state assoc
           :users (conj (:users s) new-user)
           :selected (:id new-user))))

(defn handle-update!
  "Update the selected user with name and surname"
  [state]
  (let [s @state
        id (:selected s)
        name (:name s)
        surname (:surname s)
        position (find-pos (:selected s) (:user s))]
    (when (can-update? s)
      (swap! state assoc-in
             [:users position]
             (generate-user {:id id :name name :surname surname})))))

(defn handle-delete!
  "Delete an user and switch focus to the first user"
  [state]
  (let [s @state
        position (find-pos (:selected s) (:users s))
        new-users (vec-remove position (:users s))]
    (when (can-delete? s)
      (swap! state assoc
             :users new-users
             :selected (:id (find-nearest position new-users))))))

;; ==========
;; Components
;; ==========

(defn listbox
  "Renders the listbox using a given list of users"
  [{:keys [list value on-change]}]
  [:div.row
   [:select.field.full-width {:size 3
                              :value (or value "") 
                              :on-change on-change}
    (for [user list]
      [:option {:value (:id user) :key (:id user)}
       (fullname user)])]])
            
(defonce DEMO_DATA [(generate-user {:name "Hans" :surname "Emil"})
                    (generate-user {:name "Max" :surname "Mustermann"})
                    (generate-user {:name "Tisch" :surname "Roman"})])

(defn crud [users]
  (let [state (r/atom (initial-state (or users DEMO_DATA)))]
    (fn []
      [wrapper {:title "CRUD"}
       [:div.row
        [:p "Filter prefix"]
        [:input.field {:on-change #(set-target-value state :prefix %)}]]
       [listbox {:value (:selected @state)
                 :list (filter-users (:users @state) (:prefix @state))
                 :on-change #(set-target-value state :selected %)}]
       [:div.row
        [:p "Name"]
        [:input.field {:on-change #(set-target-value state :name %)}]]
       [:div.row
        [:p "Surname"]
        [:input.field {:on-change #(set-target-value state :surname %)}]]
       [:div.buttons
        [:input {:type "button"
                 :value "Create"
                 :aria-disabled (not (can-create? @state))
                 :on-click #(handle-create! state)}]
        [:input {:type "button"
                 :value "Update"
                 :aria-disabled (not (can-update? @state))
                 :on-click #(handle-update! state)}]
        [:input {:type "button"
                 :value "Delete"
                 :aria-disabled (not (can-delete? @state))
                 :on-click #(handle-delete! state)}]]])))