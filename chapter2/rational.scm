
(define (make-rat n d)
  (define sign 
   (if (or (< n 0) (< d 0)) -1 1))
  (let  ((g (gcd (abs n) (abs d))))
    (cons (/ (* (abs n) sign) g) (/ (abs d) g)))) 

(define numer car)
(define denom cdr)

(define (add-rat x y) 
  (make-rat (+  (* (numer x) (denom y))
                (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (sub-rat x y) 
  (make-rat (-  (* (numer x) (denom y))
                (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat)))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

(newline)
(display "rationals:")
(newline)
(display "multipication => ")
(print-rat (mul-rat one-third one-third))
(newline)
(display "division => ")
(print-rat (div-rat one-third one-third))
(newline)
(display "addition => ")
(print-rat (add-rat one-third one-third))
(newline)
(display "substraction => ")
(print-rat (sub-rat one-third one-third))
(newline)
