#lang racket
;; Racket sample
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(displayln (string-append "fib(10)=" (number->string (fib 10))))
