(import love/love (defevent))
(import love/graphics)
(import love/physics)
(import love/image)
(import love/window)
(import love/keyboard)

(define anim8 :hidden (require "anim8"))
(define new-animation (.> anim8 :newAnimation))
(define new-grid (.> anim8 :newGrid))

(define scale 2)
(define foods :mutable '())

(define world :mutable nil)
(define canvas :mutable nil)
(define dog :mutable nil)

(define sheets {
  :doughnut (love/graphics/new-image "assets/strawberry.png")
  :dog (love/graphics/new-image "assets/dog-e-dog.png")
})

(define grids {
  :doughnut (new-grid
   16
   16
   (self (.> sheets :doughnut) :getWidth)
   (self (.> sheets :doughnut) :getHeight))
  :dog (new-grid
   48
   48
   (self (.> sheets :dog) :getWidth)
   (self (.> sheets :dog) :getHeight))
})

(define frames {
  :doughnut (self (.> grids :doughnut) :getFrames "1-4" 1)
  :dog (self (.> grids :dog) :getFrames "1-5" 1)
})

(define ground :mutable nil)
(defun new-ground ()
  (let* [(body (love/physics/new-body world (/ 800 2) (/ 750 2) "static"))
         (shape (love/physics/new-rectangle-shape 800 50))
         (fixture (love/physics/new-fixture body shape))]
   (self fixture :setUserData :ground)
   { :body body :shape shape :fixture fixture }))

(defun new-dog ()
  (let* [ (body (love/physics/new-body world 48 48 "kinematic"))
        (shape (love/physics/new-rectangle-shape 48 48))
        (fixture (love/physics/new-fixture body shape))
        (anim (new-animation (.> frames :dog) 0.1))]
   (self fixture :setUserData :dog)
   { :body body :shape shape :fixture fixture :anim anim }))

(defun bounded (val min max)
  (cond [(> val max) max]
        [(< val min) min]
        [true val]))

(defun set-angle (body angle-delta min-angle max-angle)
  (let* [(old-angle (self body :getAngle))
        (new-angle (bounded (+ old-angle angle-delta)
                            min-angle
                            max-angle))]
   (when (/= old-angle new-angle)
     (self body :setAngle new-angle))))

(defun new-doughnut ()
  (let* [
    (body (love/physics/new-body world 300 300 "dynamic"))
    (shape (love/physics/new-circle-shape 8))
    (fixture (love/physics/new-fixture body shape))
    (anim (new-animation (.> frames :doughnut) 0.1))
  ]
    (self body :applyLinearImpulse -8 -10)
    (self fixture :setUserData :food)
    (self body :setAngularVelocity 0.1)
    { :body body :shape shape :fixture fixture :anim anim })
)

(defun collision-with? (sym-a sym-b test-a test-b)
  (or (and (= sym-a test-a) (= sym-b test-b))
      (and (= sym-a test-b) (= sym-b test-a))))

(defun begin-contact (a b coll)
  (let [(a-body (self a :getBody))
        (a-data (self a :getUserData))
        (b-body (self b :getBody))
        (b-data (self b :getUserData))]
        (when (collision-with? :food :ground a-data b-data)
          (self b-body :setUserData true))))

(defevent :load ()
  (love/window/set-mode 800 800 { :display 2 })
  (love/physics/set-meter 64)
  (set! world (love/physics/new-world 0 (* 9.81 64) true))
  (set! ground (new-ground))
  (set! dog (new-dog))
  (set! canvas (love/graphics/new-canvas 800 800))
  (self canvas :setFilter "nearest" "nearest")

  ; callbacks?
  (self world :setCallbacks begin-contact nil nil nil)
)

(defevent :update (dt)
  (self world :update dt)
  (self (.> dog :anim) :update dt)

  (set! foods
        (reduce (lambda (foods food)
                  (self (.> food :anim) :update dt)
                  (case (self (.> food :body) :getUserData)
                    [nil (cons food foods)]
                    [true (self (.> food :body) :destroy) foods]))
                '()
                foods))

  (when (love/keyboard/is-down "space")
    (set! foods (cons (new-doughnut) foods)))
  (when (love/keyboard/is-down "w")
    (self (.> dog :body) :setY (- (self (.> dog :body) :getY) 3)))
  (when (love/keyboard/is-down "a")
    (self (.> dog :body) :setX (- (self (.> dog :body) :getX) 3)))
  (when (love/keyboard/is-down "s")
    (self (.> dog :body) :setY (+ (self (.> dog :body) :getY) 3)))
  (when (love/keyboard/is-down "d")
    (self (.> dog :body) :setX (+ (self (.> dog :body) :getX) 3)))
  (when (love/keyboard/is-down "q")
    (set-angle (.> dog :body) -0.1 -1 1))
  (when (love/keyboard/is-down "e")
    (set-angle (.> dog :body) 0.1 -1 1)))

(defun draw-dog (dog)
  (self (.> dog :anim) :draw
        (.> sheets :dog)
        (self (.> dog :body) :getX)
        (self (.> dog :body) :getY)
        (self (.> dog :body) :getAngle)
        1
        1
        15
        30))

(defevent :draw ()
  (love/graphics/set-canvas canvas)
  (love/graphics/clear)
  (draw-dog dog)

  (do [(food foods)]
    (let* [(body (.> food :body))
           (anim (.> food :anim))
           (sheet (.> sheets :doughnut))
           (x (self body :getX))
           (y (self body :getY))]
      (self anim :draw sheet x y 0 1 1 8 8)))

  (love/graphics/set-color 0.5 0.8 0.3)
  (love/graphics/polygon "fill" (self (.> ground :body)
                                      :getWorldPoints
                                      (self (.> ground :shape)
                                            :getPoints)))
  (love/graphics/set-color 1 1 1)

  (love/graphics/set-canvas)
  (love/graphics/draw canvas 0 0 0 scale scale))
