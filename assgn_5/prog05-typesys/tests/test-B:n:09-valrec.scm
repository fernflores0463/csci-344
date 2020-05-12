(val-rec [f : (int -> int)]
  ((lambda ([x : int]) (lambda ([y : int]) (+ x y)))
   (f 0)))
