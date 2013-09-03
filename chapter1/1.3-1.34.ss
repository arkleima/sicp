(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; 1.29
(define (integral f a b n)
  (define (h) (/ (- b a) n))
  (define (add-h x) (+ x (h)))

  (define (g x)
    (define (get-k) (/ (- x a) (h)))
    (define (get-multiplier y)
      (cond
       ((or (= y a) (= y n)) 1.0)
       ((odd? y) 4.0)
       ((even? y) 2.0)))

    (* (get-multiplier (get-k)) (f x)))
        
  (* (sum-acc g a add-h b) (/ (h) 3.0)))

;; 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; 1.31
;; a.
(define (product-rec term a next b)
  (if (> a b)
      1.0
      (* (term a)
         (prod-acc term (next a) next b))))

(define (factorial n)
  (define (identity x) x)
  (product-rec identity 1.0 add1 n))

(define (approximate-pi n)
  (define (add2 x) (+ x 2))
  
  (define (make-frac x)
    (*
     (/ (sub1 x) x)
     (/ (add1 x) x)))
  
  (* 4 (prod-acc make-frac 3.0 add2 n)))

;; b.
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))

  (iter a 1))

;; 1.32
;; a.
(define (acc-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (acc-rec combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (acc-iter + 0 term a next b))

(define (prod-acc term a next b)
  (acc-iter * 1 term a next b))

;; b.
(define (acc-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))

  (iter a null-value))

;; 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (square x)(* x x))
  
  (define (iter a result)  
    (define (conditional-combine a)
      (if (filter a)
          (combine a result)
          result))
    
    (if (> a b)
        result
        (iter (next a) (conditional-combine a))))

  (iter a null-value))

;; a.
(define (sum-primes a b)
  (filtered-accumulate + 0 square a add1 b prime?))

;; b.
(define (sum-rel-primes n)
  (define (identity x) x)

  (define (rel-prime? a)
    (= (gcd a n) 1))
  
  (filtered-accumulate + 0 identity 1 add1 (sub1 n) (rel-prime?)))  