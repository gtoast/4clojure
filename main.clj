; #156 - Map Defaults
(defn my-func [v coll]
  (into {} (map #(vector % v)) coll))

; #27 - Palindrome Detector
#(= (reverse %) (seq %)))

;#26 - Fibonacci Sequence
(fn fib 
  ([n] (fib n 0 1 []))
	([n a b res]
	 (if-not (> n 0)
	   res
		 (recur (dec n) b (+ a b) (conj res b)))))