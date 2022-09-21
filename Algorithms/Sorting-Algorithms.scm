#lang sicp

#|
############################
######## Algorithms ########
############################
|#


#| ################
##  Bubble Sort  ##
################ |# 


; iterates through the whole list recursivivly bubbling (faster)
(define (bsort lst)
  (define (itr lst idx)
    (define (bubble lst)
      (define (sort plst lst)
        (if (null? (cdr lst)) (reverse (cons (car lst) plst))
            (sort
             (cons (car (order-pair (car lst) (cadr lst))) plst)
             (cons (cadr (order-pair (car lst) (cadr lst))) (cddr lst)))))
      (sort '() lst))
        (if (zero? idx) lst
        (itr (bubble lst) (- idx 1))))
  (itr lst (length lst)))


; uses a boolean flag to exit when no swaps were made on a pass (slower, maybe all the equality checking?)
(define (bsort2 lst)
  (define (bubble plst lst fin)
     (cond ((and (null? (cdr lst))
                 (eq? fin #t))
                 (reverse (cons (car lst) plst)))
           ((and (null? (cdr lst))
                 (eq? fin #f))
                 (bubble '() (reverse (cons (car lst) plst)) '()))
           ((bubble
             (cons (car(car(order-pair2 (car lst) (cadr lst)))) plst)
             (cons (cadr (car(order-pair2 (car lst) (cadr lst)))) (cddr lst))
             (if (eq? fin #f) #f
                 (cdr(order-pair2 (car lst) (cadr lst))))))))
  (bubble '() lst '()))

  










#| Helpers |#

;; Get length of a list, length can do the same,
;; but we like our own procedures right?
(define l-len
  (lambda (lst)
    (if (null? lst) 0
        (+ 1 (l-len (cdr lst))))))

;; Generate random list of set amount in a range
(define l-gen-rand
  (lambda (amount range)
    (define gen
      (lambda (lst amount)
             (if (zero? amount) lst
                 (gen (cons (random range) lst) (- amount 1)))))
    (gen '() amount)))

;; Return an ordered pair from 2 values, null tests
(define order-pair
  (lambda (m n)
    (cond
      ((and (null? n) (null? m) '()))
      ((null? m) n)
      ((null? n)  m)
      ((> n m)(cons m (cons n '())))
      (else (cons n (cons m '()))))))

;; returns an ordered pair plus a boolean value
;; of whether they are already ordered or not
(define (order-pair2 n m)
  (if (> n m)(cons (cons m (cons n '())) #f)
      (cons (cons n (cons m '())) #t)))


;; Return a pair of each half of a list
(define (l-split lst)
  (define (split left right x)
    (if (zero? x) (cons (reverse left) right)
        (split (cons (car right) left) (cdr right) (- x 1))))
  (if (null? lst) '()
      (split  '() lst (round (/ (l-len lst) 2)))))
