(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                (count-leaves (cdr x))))))

(define (deep-reverse src-tree)
 (define (just-lst lst)
    (if (null? (car lst))
      '()
      (append (just-lst (icdr lst)) (car lst))))
 (if list? (car src-tree)))
