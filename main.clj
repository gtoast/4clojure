(defn my-func [v coll]
  (into {} (map #(vector % v)) coll))

; #27