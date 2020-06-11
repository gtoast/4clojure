; #156 - Map Defaults
(defn my-func [v coll]
  (into {} (map #(vector % v)) coll))

; #27 - Palindrome Detector
#(= (reverse %) (seq %))

; #26 - Fibonacci Sequence
(fn fib 
  ([n] (fib n 0 1 []))
  ([n a b res]
   (if-not (> n 0)
     res
     (recur (dec n) b (+ a b) (conj res b)))))

; #29 - Get the Caps
(fn get-the-caps 
 ([a] (get-the-caps a []))
 ([a res] 
  (if (empty? a)
    (clojure.string/join res)
    (if (java.lang.Character/isUpperCase (first a))
     (recur (rest a) (conj res (first a)))
     (recur (rest a) res)))))

; #40 - Interpose a Sequence
(fn interpo [sep coll]
  (drop-last (mapcat vector coll (repeat sep))))

; #16 Hello World
(fn [s] (str "Hello, " s "!"))

; #32 Duplicate a Sequence
(fn duplicate-a-sequence [s] (mapcat #(vector % %) s))

; #30 Compress a Sequence
(partial reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [])

; #28 Flatten a Sequence
(fn flatten-seq 
  ([s] (flatten-seq s []))
  ([s res]
   (if-let [f (first s)]
    (if (coll? f)
      (flatten-seq (next s) (concat res (flatten-seq f [])))
      (flatten-seq (next s) (concat res [f])))
    res)))

; #71 Rearranging Code: ->
last 

; #34 Implement Range
(fn my-range [a b]
  (lazy-seq
   (when (< a b)
     (cons a (my-range (inc a) b)))))

; #39 Interleave Two Seqs
(fn inter-two-seq [a b]
  (lazy-seq
   (when (and (first a) (first b))
     (concat [(first a) (first b)] (inter-two-seq (rest a) (rest b))))))

; #42 Factorial Fun
(fn fact 
 ([n] (fact n 1))
 ([n acc] 
  (if (= n 1)
   acc
   (recur (dec n) (* acc n)))))