;; (list? x)
;; returns #t if x is a proper list 
;; and returns #f otherwise.
(define list? (x)
  (if (null? x)
      #t
      (if (pair? x)
          (list? (cdr x))
          #f)))

;; (prefix? xs ys)
;; returns #t if xs is a prefix of ys (using equal? to compare elements)
;; and returns #f otherwise. 
(define prefix? (xs ys)
  (if (null? xs)
      #t
      (if (null? ys)
          #f
          (and (equal? (car xs) (car ys))
               (prefix? (cdr xs) (cdr ys))))))


(define even? (n) (= 0 (mod n 2)))
(val odd? (o not even?))
(val positive? ((curry <) 0))
(val zero? ((curry =) 0))
(val negative? ((curry >) 0))
(define flip (f) (lambda (b a) (f a b)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(val emptyset (lambda (x) #f))
(define member? (x s) (s x))

(val evens (lambda (x) (= (mod x 2) 0)))
(val two-digits (lambda (x) (and (<= 10 x) (<= x 99))))

;;;;;;;;;

(define takewhile (p? xs)
    (if (null? xs)
        '()
    ;else
        (if (p? (car xs))
            (cons (car xs) (takewhile p? (cdr xs)))
        ;else
            '()
        )
    )
)


;; test-Ea-takewhile.scm
(check-expect (takewhile negative? '()) '())
(check-expect (takewhile negative? '(-4 -3 -2 -1 0 1 2 3 4)) '(-4 -3 -2 -1))
(check-expect (takewhile negative? '(-4 -3 -2 -1)) '(-4 -3 -2 -1))
(check-expect (takewhile positive? '()) '())
(check-expect (takewhile positive? '(-4 -3 -2 -1 0 1 2 3 4)) '())
(check-expect (takewhile positive? '(-4 -3 -2 -1)) '())
(check-expect (takewhile odd? '()) '())
(check-expect (takewhile odd? '(-4 -3 -2 -1 0 1 2 3 4)) '())
(check-expect (takewhile odd? '(-4 -3 -2 -1)) '())
(check-expect (takewhile even? '()) '())
(check-expect (takewhile even? '(-4 -3 -2 -1 0 1 2 3 4)) '(-4))
(check-expect (takewhile even? '(-4 -3 -2 -1)) '(-4))
