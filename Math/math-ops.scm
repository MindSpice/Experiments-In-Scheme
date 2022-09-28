#lang sicp
(define (! n)
  (cond ((< n 0) #f)
        ((zero? n) 1)
        (else
         (* n (!(dec n))))))

(define (ncr n r)
  (if (not (>= n r 0)) #f
  (/ (! n)
     (* (! (- n r))
        (! r)))))

(define (npr n r)
  (if (not (>= n r 0)) #f
      (/ (! n)
         (! (- n r)))))

(define (fib n)
  (cond ((zero? n) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))