; #156 - Map Defaults
(defn my-func [v coll]
  (into {} (map #(vector % v)) coll))

; #27 - Palindrome Detector
#(= (reverse %) (seq %)))