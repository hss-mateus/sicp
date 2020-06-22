(define (square x) (* x x))

(define (cube x) (* x x x))

;; 1.2

(/ (+ 5 4
      (- 2 3)
      (+ 6
         (/ 4 5)))
   (* 3
      (- 6 2)
      (- 2 7)))

;; 1.3

(define (sum-square-two-largers a b c)
  "Sum the square root of the two largest numbers"

  (define (sum-square a b c)
    (+ (square a)
       (square b)
       (square c)))

  (- (sum-square a b c)
     (square (min a b c))))

;; 1.7

(define (sqrt x)

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))

  (define (good-enough? guess x)
    (< (abs (- 1 (/ guess
                    (improve guess x))))
       0.00001))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (sqrt-iter 1.0 x))

;; 1.8

(define (cubic-root x)

  (define (cubic-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cubic-root-iter (improve guess x)
                         x)))

  (define (good-enough? guess x)
    (< (abs (- 1 (/ guess
                    (improve guess x))))
       0.00001))

  (define (improve guess x)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))

  (cubic-root-iter 1.0 x))
