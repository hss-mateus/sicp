;; 1.2
(/ (+ 5 4
      (- 2 (- 3 (+ 6
                   (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; 1.3
(define (sum-two-largers x y z)

  (define sum-square
    (+ (square x)
       (square y)
       (square z)))

  (- sum-square
     (square (min x y z))))

;; 1.7
(define (sqrt x)

  (define (sqrt-iter guess)

        (define improve
            (average guess (/ x guess)))

        (define good-enough?
            (< (abs (- 1 (/ guess improve)))
               0.00001))

    (if good-enough?
        guess
        (sqrt-iter improve)))

  (sqrt-iter 1.0))

;; 1.8
(define (cube-root x)

  (define (cube-root-iter guess)

    (define improve
      (/ (+ (/ x (square guess))
            (* 2 guess))
         3))

    (define good-enough?
      (< (abs (- 1 (/ guess improve)))
         0.00001))

    (if good-enough?
        guess
        (cube-root-iter improve)))

  (cube-root-iter 1.0))

;; General functions
(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))
