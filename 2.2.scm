(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial (reverse sequence)))

(define (flatmap f xs)
  (accumulate append '() (map f xs)))

(define (enumerate-interval low high)
   (if (> low high)
       '()
       (cons low (enumerate-interval (+ low 1) high))))

;; 2.17

(define (last-pair xs)
  (if (null? (cdr xs))
      xs
      (last-pair (cdr xs))))

;; 2.18

(define (reverse xs)
  (define (iter xs acc)
    (if (null? xs)
        acc
        (iter (cdr xs) (cons (car xs) acc))))
  (iter xs '()))

;; 2.19

(define (cc amount coin-values)
  (define no-more? nil?)
  (define first-denomination car)
  (define except-first-denomination cdr)

  (cond ((= 0 amount) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

;; 2.20

(define (same-parity x . xs)
  (define (iter xs acc)
    (cond ((nil? xs) (reverse acc))
          ((or (and (even? x)
                    (even? (car xs)))
               (and (odd? x)
                    (odd? (car xs))))
           (iter (cdr xs)
                 (cons (car xs) acc)))
          (else (iter (cdr xs) acc))))
  (cons x (iter xs '())))

;; 2.21

(define (square-list xs)
  (if (null? xs)
      '()
      (cons (square (car xs))
            (square-list (cdr xs)))))

(define (square-list' xs)
  (map square xs))

;; 2.23

(define (for-each f xs)
  (map f xs)
  #t)

;; 2.25

(car (cdaddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;; 2.27

(define (deep-reverse xs)
  (define (iter xs acc)
    (cond ((null? xs) acc)
          ((pair? (car xs)) (iter (cdr xs)
                                  (cons (deep-reverse (car xs)) acc)))
          (else (iter (cdr xs)
                      (cons (car xs) acc)))))
  (iter xs '()))

;; 2.28

(define (fringe xs)
  (cond ((null? xs) '())
        ((pair? (car xs)) (append (fringe (car xs))
                                  (fringe (cdr xs))))
        (else (cons (car xs)
                    (fringe (cdr xs))))))

;; 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; 2.29a

(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

(define (mobile? x)
  (and (list? x)
       (list? (left-branch x))
       (list? (right-branch x))))

;; 2.29b

(define (total-weight x)
  (cond ((mobile? x)
         (+ (total-weight (left-branch x))
            (total-weight (right-branch x))))
        ((mobile? (branch-structure x))
         (total-weight (branch-structure x)))
        (else (branch-structure x))))

;; 2.29c

(define (balanced? x)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight branch)))

  (cond ((mobile? x)
         (= (torque (left-branch x))
            (torque (right-branch x))))
        ((mobile? (branch-structure x))
         (balanced? (branch-structure x)))
        (else #t)))

;; 2.30

(define (square-tree tree)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree' tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree' (car tree))
                            (square-tree' (cdr tree))))
        (else (square tree))))

;; 2.31

(define (tree-map f tree)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

;; 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (λ (x) (cons (car s) x)) rest)))))

;; 2.33

(define (map p xs)
  (accumulate (λ (x y) (cons (p x) y))
              '()
              xs))

(define (append xs ys)
  (accumulate cons ys xs))

(define (length sequence)
  (accumulate (λ (x y) (+ y 1)) 0 sequence))

;; 2.35

(define (count-leaves t)
  (accumulate + 0 (map (λ (x) 1) (enumerate-tree t))))

;; 2.36

(define (accumulate-n op initial xs)
  (if (null? (car xs))
      '()
      (cons (accumulate op initial (map car xs))
            (accumulate-n op initial (map cdr xs)))))

;; 2.39

(define (reverse' sequence)
  (fold-right (λ (x y) (append x (list y))) '() sequence))

(define (reverse'' sequence)
  (fold-left (λ (x y) (cons y x)) '() sequence))

;; 2.40

(define (unique-pairs n)
  (flatmap (λ (i)
             (map (λ (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; 2.41

(define (triples n s)
  (define (equal-s? triple)
    (= s (accumulate + 0 triple)))

  (filter equal-s?
          (flatmap (λ (i)
                     (flatmap (λ (j)
                                (map (λ (k) (list k j i))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
