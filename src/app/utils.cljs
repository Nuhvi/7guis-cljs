(ns app.utils)

(defn find-pos
  "Find the position of a user in a vector by their uuid"
  [id coll]
  (first (keep-indexed #(when (= (:id %2) id) %1) coll)))

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