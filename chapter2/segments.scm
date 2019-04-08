(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
 (display "(")
 (display (x-point p))
 (display ",")
 (display (y-point p))
 (display ")"))

(define (make-segment point1 point2)
  (cons point1 point2))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

(define (point-distance p1 p2)
  (sqrt (+ (sqr (- (x-point p2) (x-point p1))) (sqr (- (y-point p2) (y-point p1))))))

(define (segment-distance segment)
  (let ((x (start-segment segment)) (y (end-segment segment))) 
    point-distance x y))  


(define (mid-point segment)
  (let ((x (start-segment segment)) (y (end-segment segment)))
    (make-point (avg (x-point x) (x-point y)) (avg (y-point x) (y-point y)))))


(newline)
(display "segments :")
(newline)
(display "mid-point => ")
(print-point (mid-point (make-segment (make-point 2 4) (make-point 3 8))))
(newline)
(display "distance => ")
(print-point (segment-distance (make-segment (make-point 2 4) (make-point 3 8))))
