(define (make-interval a b) (cons a b))

(define (lower-bound a) 
  (if (< (car a) (cdr a)) 
    (car a)
    (cdr a)))

(define (upper-bound a)
  (if (< (car a) (cdr a))
   (cdr a)
   (car a)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y) 
 (make-interval (- (lower-bound x) (lower-bound y)) 
                (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (if (or (= 0 (upper-bound y)) (= 0 (lower-bound y))) (error "Division by zero"))
  (mul-interval x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))


(define (interval-radius a)
 (/ (- (upper-bound a) (lower-bound a)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p) (make-interval (+ c (* (/ c 100) p)) (- c (* (/ c 100) p))))

(define (percent a) 
  (let ((delta (- (upper-bound a) (lower-bound a)))
        (c (center a)))
    (/ (* delta 50) c)))

(newline)
(display "interval => {")
(display "lower: ")
(display (lower-bound (make-interval 0.5 2)))
(display ", upper: ")
(display (upper-bound (make-interval 0.5 2)))
(display "}")
(newline)
(display "(2,3) + (3,4) => ")
(display (add-interval (make-interval 2 3) (make-interval 3 4)))
(newline)
(display "(2,3) - (3,4) => ")
(display (sub-interval (make-interval 2 3) (make-interval 3 4)))
(newline)
(display "radius (2,3) => ")
(display (interval-radius (make-interval 2 3)))
(newline)
(display "(3,3) / (3,1) => ")
(display (div-interval (make-interval 2 3) (make-interval 3 1)))
(newline)
(display "50 士 5% => %? -> ")
(display (percent (make-center-percent 50 5)))

