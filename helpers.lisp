(import lua/math (sqrt floor random))

(defmacro pself (obj key &args)
  `(cdr (list (pcall (.> ,obj ,key) ,obj ,@args))))

(defun bounded (val min max)
  (cond [(> val max) max]
        [(< val min) min]
        [true val]))

; random value from given list
(defun sample (xs)
  (let [(index (+ 1 (floor (* (n xs) (random)))))]
    (nth xs index)))

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))

(defun vector-to (x1 y1 x2 y2)
  { :x (- x1 x2) :y (- y1 y2) })

(defun scale-vector (v scale)
  (let [(x (.> v :x))
        (y (.> v :y))]
    { :x (* scale x) :y (* scale y) }))

(defun random-range (lower upper)
  (+ lower (* (- upper lower) (random))))

(defun velocity-to-speed (velocity)
  (sqrt (+ (* (nth velocity 1) (nth velocity 1))
           (* (nth velocity 2) (nth velocity 2)))))

(defun once (fn)
  (let [(done false)]
    (lambda ()
      (when (not done)
        (fn)
        (set! done true))))
  )
