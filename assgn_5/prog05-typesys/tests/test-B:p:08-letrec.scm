(val f
  (letrec (([f : (int -> int)]
            ((lambda ([x : int]) (lambda ([y : int]) (+ x y)))
             (f 0))))
    f))
; (val ans (f 1))
