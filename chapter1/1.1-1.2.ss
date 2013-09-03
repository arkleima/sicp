(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))

; 1.11
(define (f-rec n)
  (if (< n 3)
   n
   (+ (f-rec (sub1 n))
      (* 2 (f-rec (- n 2)))
      (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (if (< n 3)
      n
      (f-iterative 2 1 0 (- n 2))))

(define (f-iterative a b c count)
  (if (= count 0)
      a
      (f-iterative (+ a (* 2 b) (* 3 c))
		   a
		   b
		   (sub1 count))))

; 1.12
(define (pascal pos level)
  (if (or (= pos 1) (= pos level))
      1
      (+ (pascal (sub1 pos) (sub1 level))
	 (pascal pos (sub1 level)))))

;; 1.16
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))

(define (expn-iter b n a)
  (cond
   ((= n 0) a)
   ((even? n) (expn-iter (square b) (/ n 2) a))
   ((odd? n) (expn-iter b (sub1 n) (* a b)))))
  
(define (fast-expn b n) (expn-iter b n 1))

;; 1.17
(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (mult-rec a b)
  (cond
   ((= b 1) a)
   ((even? b) (double (mult-rec a (halve b))))
   ((odd? b) (+ a (mult-rec a (sub1 b))))))

;; 1.18
(define (mult-iter a b acc)
  (cond
   ((= b 0) acc)
   ((even? b) (mult-iter (double a) (halve b) acc))
   ((odd? b) (mult-iter a (sub1 b) (+ a acc)))))

(define (fast-mult a b) (mult-iter a b 0))

;; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond
   ((= count 0) b)
   ((even? count)
    (fib-iter a
              b
              (+ (square p) (square q))
              (+ (square q) (* 2 p q))
              (/ count 2)))
   (else (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (sub1 count)))))

;; 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 50)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time ))

(define (search-for-primes min max)
  (cond
   ((>= min max) (timed-prime-test max))
   ((even? min) (search-for-primes (add1 min) max))
   (else
    (timed-prime-test min)
    (search-for-primes (+ min 2) max))))

;; 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;; 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (try-it a n)
    (= (expmod a n n) a))

(define (fermat-test n)  
  (try-it (+ 1 (random (- n 1))) n))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; 1.27
(define (strong-fermat-test n)
  (define (s-f-t a)
    (cond
     ((= a 1) true)
     ((try-it a n) (s-f-t (sub1 a)))
     (else false)))
  (s-f-t (sub1 n)))

;; 1.28
(define (miller-rabin-test n)
  
  (define (nt-square-root? base exp m)
    (and
      (not (= base 1))
      (not (= (fast-expn base exp) (sub1 n)))
      (= (square (fast-expn base exp)) (remainder 1 n))))

  (define (expmod-signal base exp m)
    (cond
     ((= exp 0) 1)
     ((nt-square-root? base exp m) 0)
     ((even? exp)
      (remainder (square (expmod base (/ exp 2) m))
                 m))
     (else
      (remainder (* base (expmod base (- exp 1) m))
                 m))))

  (define (m-r-t a)
    (cond
     ((>= a n) true)
     ((= (expmod-signal a (sub1 n) n) 0) false)
     ((= (expmod-signal a (sub1 n) n) 1) (m-r-t (add1 a)))
     (else false)))
         
  (m-r-t 2))