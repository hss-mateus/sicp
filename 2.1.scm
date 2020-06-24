(define (square x) (expt x 2))

(define (average a b)
  (/ (+ a b)
     2))

(define (compose f g)
  (λ (x) (f (g x))))

;; 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (and (positive? n) (positive? d))
        (cons (/ n g) (/ d g))
        (cons (- (/ n g)) (abs (/ d g))))))

;; 2.2

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

(define seg-xa
  (compose x-point start-segment))

(define seg-xb
  (compose x-point end-segment))

(define seg-ya
  (compose y-point start-segment))

(define seg-yb
  (compose y-point end-segment))

(define make-point cons)

(define x-point car)

(define y-point cdr)

(define (midpoint-segment s)
  (make-segment (average (seg-xa s)
                         (seg-xb s))
                (average (seg-ya s)
                         (seg-yb s))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

;; 2.3

(define (segment-size s)
    (sqrt (+ (square (- (seg-xa s) (seg-xb s)))
             (square (- (seg-ya s) (seg-yb s))))))

(define make-rectangle cons)

(define rec-base car)

(define rec-side cdr)

(define rec-width
  (compose segment-size rec-base))

(define rec-height
  (compose segment-size rec-side))

(define (rec-perimeter rec)
  (+ (* 2 (rec-width rec))
     (* 2 (rec-height rec))))

(define (rec-area rec)
  (* (rec-width rec)
     (rec-height rec)))

;; 2.4

(define (cons x y)
  (λ (m) (m x y)))

(define (car z)
  (z (λ (p q) p)))

(define (cdr z)
  (z (λ (p q) q)))

;; 2.5

(define (cons' a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car' z)
  (count-divisibility z 2))

(define (cdr' z)
  (count-divisibility z 3))

(define (count-divisibility x y)
  (let ((q (quotient x y))
        (r (remainder x y)))

    (if (not (= r 0))
        0
        (+ 1 (count-divisibility q y)))))

;; 2.6

(define zero
  (λ (f)
    (λ (x) x)))

(define (add-1 n)
  (λ (f)
    (λ (x)
      (f ((n f) x)))))

(define one
  (λ (f)
    (λ (x) (f x))))

(define two
  (λ (f)
    (λ (x) (f (f x)))))

(define (+' a b)
  (λ (f)
    (λ (x)
      ((b f) ((a f) x)))))
