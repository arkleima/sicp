(define (zero? n) (= n 0))
(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; 1.35
(define (golden-ratio) (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; 1.36
(define (average x y) (/ (+ x y) 2.0))
(define (sol) (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
(define (sol-damping) (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))

;; 1.37
(define (cont-frac n d k)
  (define (cont-frac-rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+
                  (d i)
                  (cont-frac-rec (add1 i))))))

  (define (cont-frac-iter i result)
    (if (zero? i)
        result
        (cont-frac-iter (sub1 i) (/ (n i) (+ (d i) result)))))
  
  (cont-frac-rec 1))

;; 1.38
(define (divisible-by-3? n)
  (= (remainder n 3) 0))

(define (compute-e)
  (+ 2
     (cont-frac (lambda (x) 1.0)
                (lambda (x)
                  (let ((n (add1 x)))
                   (if (divisible-by-3? n)
                       (* 2 (/ n 3))
                       1)))
                50)))

;; 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= 1 i)
                   x
                   (* -1 (square x))))
             (lambda (i) (sub1 (* 2 i)))
             k))