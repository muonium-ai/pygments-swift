;; Emacs Lisp sample
(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 1)) (fib (- n 2)))))

(message "fib(10)=%s" (fib 10))
