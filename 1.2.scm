(define (even? x)
  (= (remainder x 2) 0))

;; 1.11

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2
                    (f (- n 2)))
                 (* 3
                    (f (- n 3)))))))

(define (f' n)

  (define (f-iter a b c counter)
    (if (= counter 0)
        c
        (f-iter (+ a
                   (* 2 b)
                   (* 3 c))
                a
                b
                (- counter 1))))

  (f-iter 2 1 0 n))

;; 1.16

(define (exp b n)

  (define (square x) (* x x))

  (define (exp-iter b counter product)
    (cond ((= counter 0) product)
          ((even? counter) (exp-iter (square b)
                                     (/ counter 2)
                                     product))
          (else (exp-iter b
                          (- counter 1)
                          (* b product)))))

  (exp-iter b n 1))

;; 1.17

(define (* a b)

  (define (double x) (+ x x))

  (define (halve x) (/ x 2))

  (define (*-iter a counter sum)
    (cond ((= counter 0) sum)
          ((even? counter) (*-iter (double a)
                                   (halve counter)
                                   sum))
          (else (*-iter a
                        (- counter 1)
                        (+ a sum)))))

  (*-iter a b 0))
