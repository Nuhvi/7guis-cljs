(ns app.utils)

(defn numeric?
  "Check that input string is a valid Number"
  [string]
  (not (js/Number.isNaN (js/Number string))))
