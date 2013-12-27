; 2.1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (positive? n)
  (>= n 0))

(define (negative? n)
  (> 0 n))

(define (make-rat n d)
  (define (sign-multiplier num den)
    (if (and (negative? num) (positive? den))
        -1
        1))
  
  (let ((g (gcd n d))
        (sign (sign-multiplier n d)))
    (cons (* sign (/ n g)) (* sign (/ d g)))))
