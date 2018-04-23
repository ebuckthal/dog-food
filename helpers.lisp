(import lua/math)

(defun bounded (val min max)
  (cond [(> val max) max]
        [(< val min) min]
        [true val]))

; random value from given list
(defun sample (xs)
  (let [(index (+ 
                 1 
                 (lua/math/floor 
                   (* (n xs) 
                      (lua/math/random)))))]
    (nth xs index)))
