#lang sicp

; Get length of a list
(define l-len
  (lambda (lst)
    (if (null? lst) 0
        (+ 1 (l-len (cdr lst))))))


; Get the sum of a list
 (define l-sum
   (lambda (lst)
     (if (null? lst) 0
         (+ (car lst) (l-sum (cdr lst))))))


; Avg of list
(define (l-avg lst)
  (/ (l-sum lst) (l-len lst)))


; Pass in a operation sybol as f for multi functionality *limited use cases
(define l-op
  (lambda (f dvalue lst)
    (if (null? lst) dvalue
        (f (car lst) (l-op f dvalue (cdr lst))))))

; Find if a list contains a value
(define l-con
  (lambda (lst value)
    (cond ((null? lst) #f)
          ((= (car lst) value) #t)
          (else (l-con (cdr lst) value)))))
		  

; Find if a list contains a value
(define l-con-idx
  (lambda (lst value)
    (define coni
      (lambda (lst value idx)
        (cond ((null? lst) #f)
              ((= (car lst) value) (cons #t idx))
              (else (coni (cdr lst) value (+ idx 1))))))
    (coni lst value 0)))


; Retrieve list value by index
(define l-geti
  (lambda (lst idx)
    (cond ((null? lst) #f)
          ((= 0 idx) (car lst))
          (else (l-geti (cdr lst) (- idx 1))))))

               
; Generate an ordered list
(define l-gen-range
  (lambda (lbound ubound)
    (cond ((zero? (-  ubound lbound)) '())
          (else
           (cons (+ lbound 1)
                 (l-gen-range (+ lbound 1) ubound))))))


; Generate random list 
(define l-gen-rand
  (lambda (amount range)
    (define gen
      (lambda (lst amount)
             (if (zero? amount) lst
                 (gen (cons (random range) lst) (- amount 1)))))
    (gen '() amount)))


; Generate random list way style of Little Schemer mcuh slower (5-10x)
(define ll-gen-rand
  (lambda (amount range)
    (cond ((zero? amount) '())
          (else
           (cons (random range)
                 (ll-gen-rand (- amount 1) range))))))

                    
; Replace element in a list at index
(define l-rplc
  (lambda (lst idx value)
    (define rplc
      (lambda (lst idx value l-lst)
        (cond ((null? lst) #f)
              ((zero? idx) (append (reverse (cons value l-lst)) (cdr lst)))
              (else
               (rplc (cdr lst) (- idx 1) value (cons (car lst) l-lst) )))))
    (rplc lst idx value '())))


; This style is quite slower (2-2.5x), but is of the style of Little Schemers remove operation 
(define ll-rplc
  (lambda (lst idx value)
    (cond ((null? lst) '())
          ((= 0 idx) (cons value (cdr lst)))
          (else
           (cons (car lst)
                 (ll-rplc (cdr lst) (- idx 1) value))))))

 
; Romove atom from list by value
(define (l-remv lst value)
       (cond ((null? lst) '())
              ((eq? (car lst) value) (cdr lst))
              (else
               (cons (car lst)
               (l-remv (cdr lst) value)))))
  
                            
; Insert element in a list at index
(define l-insert
  (lambda (lst idx value)
    (define insert
      (lambda (lst idx value l-lst)
        (cond ((null? lst) #f)
              ((zero? idx) (append (reverse (cons value l-lst))  lst))
              (else (insert (cdr lst) (- idx 1) value (cons (car lst) l-lst))))))
    (insert lst idx value '())))


; Slower (2-2.5x) insert using more minimalistic little scheme style recurision 
(define ll-insert
  (lambda (lst idx value)
         (cond ((null? lst) '())
               ((zero? idx) (cons value lst))
               (else
                (cons (car lst)
                      (ll-insert (cdr lst) (- idx 1) value))))))


; Return a pair of each half of a list
(define (l-split lst)
  (define (split left right x)
    (if (zero? x) (cons (reverse left) right)
        (split (cons (car right) left) (cdr right) (- x 1))))
  (if (null? lst) '()
      (split  '() lst (round (/ (l-len lst) 2)))))


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

