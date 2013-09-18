(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))

(define tolerance 0.00001)
(define (average x y) (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; 1.40
(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;; 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; 1.43
(define (repeated f n)
  (if (= 1 n)
      f
      (compose f (repeated f (sub1 n)))))

;; 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx))
                    (f x)
                    (f (- x dx)))
                 3)))

(define (n-fold-smoothed f n)
  (repeated (smooth f) n))

;; 1.45
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))

(define (expn-iter b n a)
  (cond
   ((= n 0) a)
   ((even? n) (expn-iter (square b) (/ n 2) a))
   ((odd? n) (expn-iter b (sub1 n) (* a b)))))
  
(define (fast-expn b n) (expn-iter b n 1))

(define (nth-root x n)
  (define (n-sub1-pow y)
    (fast-expn y (sub1 n)))
  
  (let ((repetitions 2))
  (fixed-point (repeated
                (average-damp (lambda (y) (/ x (n-sub1-pow y))))
                repetitions)
               1.0)))

;; 1.46
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (if (good-enough? x)
        x
        ((iterative-improve good-enough? improve) (improve x)))))

(define (sqrt-good x)
  ((iterative-improve
   (lambda (y) (< (abs (- (square y) x)) 0.001))
   (lambda (z) (average z (/ x z)))) 1.))

(define (fixed-point-good f first-guess)
  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  ;; There must be some way of making this easier to read...
  ((iterative-improve
    (lambda (x) (close-enough? x (f x)))
    f)
   first-guess))
                
