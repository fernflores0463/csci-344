;; Name: Fernando Flores
;; Time spent on assignment: 5 hours
;; Collaborators: 


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part A (Exercise 2)

;; A.a (Exercise 2a)
;; DEFINE count HERE
;; Return the number of top level elements of xs that are equal to x
(define count (x xs)
    (if (null? xs) 
        0
    ;;else
        (if (equal? x (car xs))
            (+ 1 (count x (cdr xs))) 
        ;;else
            (count x (cdr xs))
        )
    )
)

;; A.b (Exercise 2b)
;; DEFINE countall HERE
(define countall (x xs)
    (if (null? xs) 
        0
    ;;else
        ;;if the curr element is an atom
        (if (atom? (car xs) ) 
            ;;then check if atom is equal to x
            (if (equal? x (car xs))
                ;;then increment total by one
                (+ 1 (countall x (cdr xs)))
            ;;else
                ;;continue recursion
                (countall x (cdr xs))
            )
        ;;else
            ;;go into the nonatomic current value
            ( countall x (car xs) )
        )
    )
)
;; A.c (Exercise 2c)
;; DEFINE mirror HERE


(define mirror (xs)
    (if(list? xs)  
        (reverse (map mirror xs))
    ;;else
        ;;reverse the list
        xs
    )
)

;; A.d (Exercise 2d)
;; DEFINE flatten HERE

(define flatten (lst)
  (if (null? lst) lst
    (if (list? (car lst))
        (append (flatten (car lst)) (flatten (cdr lst)))

        (cons (car lst) (flatten (cdr lst)))
    )
  )
)

;; A.e (Exercise 2e)
;; DEFINE sublist? HERE

(define sublist?(xs ys)
    (if (null? xs)
        #t
        (if (null? ys)
            #f
            (if (prefix? xs ys)
                #t
                (sublist? xs (cdr ys))
            )
        )
    )
)

;; A.f (Exercise 2f)
;; DEFINE subseq? HERE

(define subseq?(xs ys)
    (if (null? xs)
        #t
        (if (null? ys)
            #f
            (if (equal? (car xs) (car ys) )
                (subseq? (cdr xs) (cdr ys))
            ;;else
                (subseq? xs (cdr ys))
            )
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part B

;; B.1 (take)
;; DEFINE take HERE
(define take(n xs)
    (if (or (null? xs) (= n 0))
        '()
    ;else
        (cons (car xs) (take (- n 1) (cdr xs)))
    )
)

;; B.2 (drop)
;; DEFINE drop HERE
(define drop(n xs)
    (if (or (null? xs) (= n 0))
        xs
    ;else
        (append '() (drop (- n 1) (cdr xs)))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part C (interleave)

;; DEFINE interleave HERE
(define interleave(xs ys)
    (if (null? xs)
        ys
    ;else 
        (if (null? ys)
            xs
            (cons (car xs) (interleave (cons (car ys) (cdr ys)) (cdr xs)))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part D (permutation?) !bonus!

;; DEFINE permutation? HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part E (Exercise 10)

;; E.a (Exercise 10a)
;; DEFINE takewhile HERE
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

;; E.b (Exercise 10b)
;; DEFINE dropwhile HERE
(define dropwhile (p? xs)
    (if (null? xs)
        xs
    ;else
        (if (p? (car xs))
            (dropwhile p? (cdr xs))
        ;else
            xs
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part F (arg-max)

;; DEFINE arg-max HERE
(define helper-arg-max (f curr xs)
    (if (null? xs)
        curr
    ;else
        (if (> (f (car xs)) (f curr))
            (helper-arg-max f (car xs) (cdr xs))
            (helper-arg-max f curr (cdr xs))
        )
    )
)

(define arg-max (f xs)
    (helper-arg-max f (car xs) xs)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part G (Exercise 14)

;; G.b (Exercise 14b)
;; DEFINE max* HERE
(define max* (xs)
    (foldl max (car xs) xs)
 )

;; G.c (Exercise 14c)
;; DEFINE gcd* HERE
(define gcd* (xs)
(foldl gcd (car xs) xs)
)

;; G.g (Exercise 14h)
;; DEFINE append-via-fold HERE
(define append-via-fold (xs ys)
    (if (null? xs)
        ys
        (if (null? ys)
            xs
            (foldl cons ys (reverse xs))
        )
    )
)

;; G.i (Exercise 14j)
;; DEFINE reverse-via-fold HERE
(define reverse-via-fold (xs)
    (if (null? xs)
        xs
    ;else    
        (foldl cons '() xs)
    )
)

;; G.j (Exercise 14k)

(define insert (x xs)
  (if (null? xs)
      (list1 x)
      (if (< x (car xs))
          (cons x xs)
          (cons (car xs) (insert x (cdr xs))))))

;; DEFINE insertion-sort HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part H (Exercise 19)

(val emptyset (lambda (x) #f))
(define member? (x s) (s x))

(val evens (lambda (x) (= (mod x 2) 0)))
(val two-digits (lambda (x) (and (<= 10 x) (<= x 99))))

;; H.1 (Exercise 19c)
;; DEFINE add-element HERE
(define add-element (e xs)
    (if (member? e xs)
        xs
    ;else
        (lambda(x) (if (= e x) #t (member? x xs)))
    )
)
;; H.2 (Exercise 19c)
;; DEFINE union HERE


;; H.3 (Exercise 19c)
;; DEFINE inter HERE

;; H.4 (Exercise 19c)
;; DEFINE diff HERE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part I

;; DEFINE clamp HERE
(define clamp (f low high)
    (lambda (x)
        (if (< (f x) low)
            low
            (if (> (f x) high)
                high
                (f x)
            )
        )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part J

;; DEFINE balanced? HERE

