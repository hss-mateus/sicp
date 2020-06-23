(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (inc-by x)
  (λ (y) (+ x y)))

(define (average x y)
  (/ (+ x y) 2))

(define (prime? x)
  (define (iter n)
    (cond ((< x 2) #f)
          ((= n x) #t)
          ((= 0 (remainder x n)) #f)
          (else (iter (inc n)))))
  (iter 2))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; 1.31a

(define (product term a next b)

  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))

  (iter a 1))

(define (factorial x)
  (product identity 1 inc x))

(define (pi-aprox n)
  (* 4.0
     (/ (* 2 n (product square 4 (inc-by 2) (- n 1)))
        (product square 3 (inc-by 2) n))))

;; 1.31b

(define (product' term a next b)
  (if (> a b)
      1
      (* (term a) (product' term (next a) next b))))

;; 1.32a

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))

  (iter a null-value))

(define (sum' term a next b)
  (accumulate + 0 term a next b))

(define (product'' term a next b)
  (accumulate * 1 term a next b))

;; 1.32b

(define (accumulate' combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate' combiner null-value term (next a) next b))))

;; 1.33

(define (filtered-accumulate combiner null-value term a next b filter)
  (define filtered-value
    (if (filter a)
        (term a)
        null-value))

  (if (> a b)
      null-value
      (combiner filtered-value (filtered-accumulate combiner null-value term (next a) next b filter))))

;; 1.33a

(define (sum-square-primes-between a b)
  (filtered-accumulate + 0 square a inc b prime?))

;; 1.33b

(define (product-relatively-primes n)
  (define (filter x)
    (and (< x n)
         (= 1 (gcd n x))))

  (filtered-accumulate * 1 identity 1 inc n filter))

;; 1.35

(define (golden-ratio x)
  (fixed-point (λ (y) (+ 1 (/ 1 y)))
               1.0))

;; 1.36

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))

  (define (try guess)
    (display guess)
    (newline)

    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))

(fixed-point (λ (x) (/ (log 1000)
                       (log x)))
            2.0)

;; 1.37a

(define (cont-frac n d k)
  (define (aux x)
    (if (= k x)
        (d k)
        (let ((n-value (n k))
              (d-value (d k)))
          (+ d-value
             (/ n-value
                (aux (inc x)))))))

  (/ (n 1) (aux 2)))

;; 1.37b

(define (cont-frac' n d k)
  (define (iter k acc)
    (if (= k 1)
        acc
        (let ((n-value (n k))
              (d-value (d k)))
          (iter (- k 1)
               (/ n-value
                  (+ d-value acc))))))

  (iter (- k 1) (d k)))

;; 1.41

(define (double f)
  (λ (x) (f (f x))))

;; 1.42

(define (compose f g)
  (λ (x) (f (g x))))

;; 1.43

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

;; 1.44

(define (smooth f)
  (define (average x y z) (/ (+ x y z) 3))

  (let ((dx 0.00001))
    (λ (x) (average (f (- x dx))
                    (f x)
                    (f (+ x dx))))))

(define (repeated-smooth f n)
  (repeat (smooth f) n))

;; 1.46

(define (iterative-improve good-enough? improve)
  (λ (guess)
    (define (iter guess)
        (if (good-enough? guess)
            guess
            (iter (improve guess))))
    (iter guess)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00001))

  (define (improve guess)
    (average (/ x guess) guess))

  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point' f first-guess)
  (define (good-enough? x)
    (< (abs (- x (f x))) 0.00001))

  ((iterative-improve good-enough? f) 1.0))
