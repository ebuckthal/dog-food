(import lua/math (sqrt floor random))

(define flux :hidden (require "flux"))
(define flux/to (.> flux :to))

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

(defmacro make-tween (name next obj time newobj)
  `(defun ,name ()
     (self (self (flux/to ,obj ,time ,newobj)
                 :oncomplete ,next)
           :ease "sineinout")))

;; this function must return a function that can be called
;; pcall(shape:getPoint)
;; pcall(shape.getPoint, shape)
(defun partial (fn &args)
  (lambda (&moreargs) (apply fn (append args moreargs))))

; self-wrappers
(defun body/get-x (body) (self body :getX))
(defun body/get-y (body) (self body :getY))

(defun body/get-speed (body)
  (velocity-to-speed (pself body :getLinearVelocity)))

(defun body/vector-to (body x y)
  (vector-to (body/get-x body) (body/get-y body) x y))

(defun body/distance-to (body x y)
  (distance (body/get-x body) (body/get-y body) x y))

(defun body/apply-linear-impulse-vector (body v)
  (self body :applyLinearImpulse (.> v :x) (.> v :y)))

