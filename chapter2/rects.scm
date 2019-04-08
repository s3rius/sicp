(define (make-rect1 a b) (cons a b))

(define (rect-len-a rect) (car rect))
(define (rect-len-b rect) (cdr rect))

(define (sqare rect) ( * (rect-len-a rect) (rect-len-b rect)))

(define (perimeter rect) (* ( + (rect-len-a rect) (rect-len-b rect)) 2))
