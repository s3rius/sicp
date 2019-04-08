(define (make-powers a b) (cons (pow 2 a) (pow 3 b)))

(define (first-powers a) (car a))
(define (second-powers a) (cdr a))

(define (multiply-powers a) ( * (first-powers a) (second-powers a)))

(newline)
(display "powers => ")
(display (multiply-powers (make-powers 2 3)))
(newline)
