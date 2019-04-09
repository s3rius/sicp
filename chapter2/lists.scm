(define (last-pair lst) (list-ref lst (- (length lst) 1)))

(define test-list (list 5 4 3 2 1))

(define (reverse-lst lst)
  (define (helper remaining result)
    (if (null? remaining)
     result
     (helper (cdr remaining) (append result (list (car remaining))))))
  (helper lst ()))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? lst) (= 0 (length lst)))

(define (first-denomination lst) (list-ref lst 0))

(define (except-first-denomination lst) (cdr lst))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
              (except-first-denomination coin-values))
            (cc (- amount
                  (first-denomination coin-values))
              coin-values)))))

(define (filter lst pred)
  (define (iffer el)
   (if (pred el) (list el) ())) 
  (define (helper remain result)
    (if (= 0 (length remain))
     result
     (helper (cdr remain) (append result (iffer (car remain))))))
  (helper lst ()))
         


(define (same-parity x . l) 
  (define (find-pred)
   (if (divides? x 2)
      (lambda (y) (divides? y 2))
      (lambda (y) (not (divides? y 2)))))
  (filter (append (list x) l) (find-pred)))

(define (for-each action lst)
   (define (iterate remain)
    (if (not (null? remain))
      (action (car remain)))
    (if (not (= 0 (length remain)))
      (iterate (cdr remain))
      ()))
   (iterate lst))
  


(newline)
(display "Lists:")
(newline)
(display "test-list => ")
(display test-list)
(newline)
(display "length => ")
(display (length test-list))
(newline)
(display "last el => ")
(display (last-pair test-list))
(newline)
(display "Reversed => ")
(display (reverse-lst test-list))
(newline)
(display "15 US coins cc => ")
(display (cc 15 us-coins))
(newline)
(display "15 UK coins cc => ")
(display (cc 15 uk-coins))
(newline)
(display "Same parity => ")
(display (same-parity 3 5 4 3 2 1))
(newline)
(display "Sqare list => ")
(display (map sqr test-list))
(newline)
(display "Print list elements => {")
(for-each (lambda (x) (display x) (display ",")) test-list)
(display "}")
(newline)
