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

(define sheets {
  :doughnut (love/graphics/new-image "assets/doughnut.png")
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

(defun new-doughnut ()
  (let* [
    (body (love/physics/new-body world 300 300 "dynamic"))
    (shape (love/physics/new-circle-shape 8))
    (fixture (love/physics/new-fixture body shape))
    (anim (new-animation (.> frames :doughnut) 0.1))
  ]
    (self body :applyLinearImpulse -8 -10)
    (self fixture :setUserData "ball")
    (self body :setAngularVelocity 0.1)
    { :body body :shape shape :fixture fixture :anim anim })
)

(defevent :load ()
  (love/window/set-mode 800 800 { :display 2 })
  (love/physics/set-meter 64)
  (set! world (love/physics/new-world 0 (* 9.81 64) true))
  (set! canvas (love/graphics/new-canvas 800 800))
  (self canvas :setFilter "nearest" "nearest")
)

(defevent :update (dt)
  (self world :update dt)

  (do [(food foods)]
    (self (.> food :anim) :update dt))

  (when (love/keyboard/is-down "space") 
    (set! foods (cons (new-doughnut) foods)))
)

(defevent :draw ()
  (love/graphics/set-canvas canvas)
  (love/graphics/clear)

  (do [(food foods)]
    (let* [(body (.> food :body))
          (anim (.> food :anim))
          (sheet (.> sheets :doughnut))
          (x (self body :getX))
          (y (self body :getY))]
      (self anim :draw sheet x y 0 1 1 8 8)))

  (love/graphics/set-canvas)
  (love/graphics/draw canvas 0 0 0 scale scale)
)
