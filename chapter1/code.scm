; TODO: Add loading tests

(define (iter_fact n)
 (define (iter count val)
   (cond ((> count n) val)
     (else
       (iter (+ count 1) (* val count)))))
 (iter 1 1))

(define (ffact n)
 (cond ((= n 1) 1)
   (else
     (rec_fact (- n 1)))))

(define (count-change amount)
 (cc amount 5))

(define (cc amount kinds)
 (cond ((= amount 0) 1)
       ((or (< amount 0) (= kinds 0)) 0)
       (else ( + (cc amount (- kinds 1)) (cc (- amount (coin-kind kinds)) kinds)))))

(define (coin-kind n)
 (cond ((= n 1) 1)
       ((= n 2) 5)
       ((= n 3) 10)
       ((= n 4) 25)
       ((= n 5) 50)))

(define (rec_func n)
 (cond ((< n 3) n)
       (else (rec_func (- n 1)) + (rec_func (- n 2)) + (rec_func (- n 3)))))

(define (even? a)
 (= (remainder a 2) 0))

(define (sqr a)
 (* a a))

(define (pow a b n)
 (cond ((= n 0) b)
       (else (pow a (* b a) (- n 1)))))

(define (fast_rec_pow a n)
 (cond ((= n 0) 1)
       ((even? n) (sqr (fast_rec_pow a (/ n 2))))
       (else (* a (fast_rec_pow a (- n 1))))))

(define (fast_iter_pow c g)
 (define (iter_pow b n a)
   (cond ((= n 0) a)
         ((even? n) (iter_pow (* b b) (/ n 2) a))
         (else (iter_pow b (- n 1) (* a b)))))
 (iter_pow c g 1))

(define (gcb a b)
 (if (= b 0)
     a
     (gcb b (remainder a b))))

(define (find_divisor num testing)
 (cond ((> (sqr testing) num) num)
       ((divides? num testing) testing)
       (else (find_divisor num (+ testing 1)))))


(define (divides? a b)
 (= (remainder a b) 0))

(define (smallest_divisor num)
 (find_divisor num 2))

(define (prime? num)
 (= (smallest_divisor num) num))

(define (cube a)
 (* a a a))

(define (simpson f a b n)
; Simpson Integration Formula
 (define (get_h) (+ a (/ (- b a) n)))
 (define (integrate h na nb pn)
   (cond ((> na nb) 0)
         ((even? pn) (+ (* (f na) 2) (integrate h (+ na h) nb (+ pn 1))))
         (else (+ (* (f na) 4) (integrate h (+ na h) nb (+ pn 1))))))
 (* (+ (integrate (get_h) (+ a (get_h)) (- b (get_h)) 1) (f a) (f b)) (/ (get_h) 3)))

(define (id x) x)
(define (inc a) (+ a 1))

; (define (sum term a next b)
;   (define (iter a result)
;     (if (a > b)
;       (0)
;       (iter (next a) (+ result (term a)))))
;   (iter (next a) (term a)))

(define (sum term a next b)
 (define (iter a result)
   (if (> a b)
     result
     (iter (next a) (+ result (term a)))))
 (iter (next a) (term a)))


(define (sum-integers a b)
 (sum id a inc b))


(define (product_rec term a next b)
  (if (> a b)
    1
    (* (term a) (product_rec term (next a) next b))))

(define (product_iter term a next b)
 (define (iter a result)
   (if (> a b)
     result
     (iter (next a) (* result (term a)))))
 (iter (next a) (term a)))

(define (product_rec_int a b)
  (product_rec id a inc b))

(define (product_iter_int a b)
 (product_iter id a inc b))

(define (step2 a) (+ a 2))

(define (aproxPi b)
 (* 2 (/ (product_rec sqr 4 step2 b) (product_rec sqr 3 step2 b))))

(define (acc combiner null_val term a next b)
  (if (> a b)
    null_val
    (combiner (term a) (acc combiner null_val term (next a) next b))))

(define (positive? a) (if (> a 0) #t #f))

(define (negative? a) (if (< a 0) #t #f))

(define (filtered_acc combiner null_val predicate term a next b)
  (cond ((> a b) null_val)
        ((predicate a) (combiner (term a) (filtered_acc combiner null_val predicate term (next a) next b)))
        (else null_val)))

(define (avg a b)
 (/ (+ a b) 2))

(define (close_enough? x y) (if (< (abs (- x y)) 0.001) #t #f))

(define (search f neg pos)
 (let ((middle (avg neg pos)))
   (if (close_enough? neg pos)
       middle
       (let ((test_val (f middle)))
           (cond ((positive? test_val)
                  (search f neg middle))
                 ((negative? test_val)
                  (search f middle pos))
                 (else middle))))))

(define (half-interval f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (negative? a-val) (positive? b-val))
           (search f a-val b-val))
          ((and (positive? a-val) (negative? b-val))
           (search f b-val a-val))
          (else (error "Fuck")))))

(define tolerance 0.0001)


(define (finite_con_frac n d k)
 (define (try i)
   (if (= i k)
     1.0
     (+ (d (- i 1)) (/ (n i) (try (+ i 1))))))
 (/ (n 1.0) (try 2.0)))


(define (finite_exp k)
 (define (d_val i)
   (cond ((= i 1) 1)
         ((= i 2) 2)
         ((divides? (- i 2) 3 ) (+ (d_val (- i 1)) (d_val (- i 2)) (d_val (- i 3))))
         (else 1)))
 (+ 2 (finite_con_frac (lambda (x) 1) d_val k)))


(define (finite_tan rad k)
 (define (d_val i)
   (if (= i 1)
       1
       (+ 2 (d_val(- i 1)))))
 (define (n_val i)
   (if (= i 1)
     rad
     (- (* rad rad))))
 (finite_con_frac n_val d_val k))

(define (avg-damp f)
 (lambda (x) (avg x (f x))))

(define (sqrt x)
 (fixed_point (avg-damp (lambda (y) (/ x y))) 1.0))

(define (cube_sqrt x)
 (fixed_point (avg-damp (lambda (y) (/ x (sqr y)))) 1.0))


(define (deriv f)
 (define dx 1e-9)
 (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newtons-method g guess)
 (define (newton_trans g)
   (lambda (x)
     (- x (/ (g x) ((deriv g) x)))))
 (fixed_point (newton_trans g) guess))

(define (cubic a b c)
 (lambda (x)
   (+ (cube x) (* a (sqr x)) (* b x) c)))

(define (double f)
 (lambda (x) (f (f x))))

(define (compose f g)
 (lambda (x) (f (g x))))

(define (repeated f n)
 (define (repeat k)
   (if (<= k 0)
     f
     (compose f (repeat (- k 1)))))
 (repeat (- n 1)))

(define (pow a b)
 (define (helper n res)
   (if (= n b)
     res
     (helper (+ n 1) (* res a))))
 (helper 0 1))

(define (nsqrt x n)
 (define (relation y)
   (/ x (pow y (- n 1))))
 (fixed_point ((repeated avg-damp (- n 1) relation) 1.0)))

(define (iterative_improve is_ok? improve)
 (define (guesser y)
   (let ((new_val (improve y)))
     (if (is_ok? y new_val)
       new_val
      (guesser (improve new_val)))))
 guesser)


(define (fixed_point f first_guess)
  (define (close? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative_improve close? f) first_guess))

(define main 
  (newline)
  (display "-----------------------------")
  (newline)  
  (display "Chapter loading successfull.")
  (newline)
  (display "-----------------------------")
  0)
