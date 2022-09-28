#lang sicp

#|
############################
######## Algorithms ########
############################
|#


#| ################
##  Bubble Sort  ##
################ |#

; Iterates through the whole list recursively bubbling (faster)
;; also seems to be the fastest implimentation
;; *Uses the order-pair help to return an order pair of n m

; fastest implimentation

(define (bsort lst)
  (define (bubble lst)
    (define (sort plst lst)
      (if (null? (cdr lst)) (reverse (cons (car lst) plst))
          (sort
           (cons (car (order-pair (car lst) (cadr lst))) plst)
           (cons (cadr (order-pair (car lst) (cadr lst))) (cddr lst)))))
        (sort '() lst))
    (define (itr lst idx)
    (if (zero? idx) lst (itr (bubble lst) (- idx 1))))
  (itr lst (length lst)))

; Cleanest implimentation, Iterative, slightly slower than the bsort above

(define (bsort2 lst)
  (define (sort lst)
    (cond ((null? (cdr lst))
           (cons (car lst) '()))
          (else
           (cons (car(order-pair (car lst) (cadr lst)))
                 (sort (cons(cadr(order-pair (car lst) (cadr lst)))(cddr lst)))))))
  (define (itr lst x)
    (if (zero? x) lst
        (itr (sort lst) (dec x))))
  (itr lst (length lst)))
    

; uses a boolean flag to exit when no swaps were made on a pass
; slower, likely all the equality checking? and multiple order call
; needs more re-worked

(define (bsort3 lst)
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


#| ###################
##  Selection Sort  ##
################### |#

;; *Uses the helper remove procedure from self-made list operations

(define (ssort lst)
  (define (get-low lst low)
    (cond ((null? lst) low)
          ((< (car lst) low)
           (get-low(cdr lst) (car lst)))
          (else
           (get-low (cdr lst) low))))
  (cond ((null? lst) '())
        (else
         (cons (get-low lst (car lst))
               (ssort (l-remv lst (get-low lst (car lst))))))))

#|(define (ssort2 lst)
  (define llst lst)
  (define (itr plst lst)
    (define (get-low lst low plst)
      (cond ((null? lst) (cons low plst))
            ((< (car lst) 
                (get-low(cdr lst) (car lst)))
             (else
              (get-low (cdr lst) low)))))
    (if (= (length list) (get-low (itr)))))) |#


(define (msort lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) (car lst))
          (else
           (cons (car (order-pair (msort (car (l-split (cdr lst))))
                                  (msort (cdr (l-split (cdr lst))))))
                 (msort (cddr lst))))))




; Return a list with all duplicates removed

(define (l-rem-dupes lst)
  (define (look-fwd lst value)
    (cond ((null? lst) '())
          ((eq? (car lst) value)
           (look-fwd (cdr lst) value))
          (else
           (cons (car lst)
                 (look-fwd (cdr lst) value)))))
  (if (null? lst) '()
      (cons (car lst)
            (l-rem-dupes (look-fwd lst (car lst))))))



;; perform a right rotate on a list
(define (l-rotate-r lst steps)
  (define (get-last lst)
    (cond ((null? (cdr lst)) (cons (car lst) '()))
          (else (get-last (cdr lst)))))
  (define (drop-last lst)
    (if (null? (cdr lst)) '()
        (cons (car lst)
              (drop-last (cdr lst)))))
  (if (zero? steps) lst
      (l-rotate-r (cons (car (get-last lst)) (drop-last lst)) (dec steps))))





 
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
      ((null? m) (cons n '()))
      ((null? n)  (cons m '()))
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

(define (l-remv lst value)
       (cond ((null? lst) '())
              ((eq? (car lst) value) (cdr lst))
              (else
               (cons (car lst)
               (l-remv (cdr lst) value)))))
