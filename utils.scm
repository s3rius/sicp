
(define (logoprint text)
  (newline)
  (display "-----------------------------")
  (newline) 
  (display text)
  (newline)
  (display "-----------------------------"))

(define (n_to_s i) (number->string i))


(define (loading-chapter n)
  (logoprint (string-append "Chapter " (n_to_s n) " loading successfull.")))
