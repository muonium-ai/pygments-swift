;; Fibonacci (Clojure)

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(doseq [i (range 0 12)]
  (println i (fib i)))
