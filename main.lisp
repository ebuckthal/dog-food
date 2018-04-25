(import love/love (defevent))
(import love/graphics)
(import love/physics)
(import love/image)
(import love/window)
(import love/keyboard)
(import love/audio)

; import from helpers.lisp
(import helpers (bounded sample))

(define love :hidden (require "love"))
(define anim8 :hidden (require "anim8"))
(define new-animation (.> anim8 :newAnimation))
(define new-grid (.> anim8 :newGrid))

(define scale 2)

; game objects

(define world :mutable nil)
(define canvas :mutable nil)
(define ground :mutable nil)
(define dog :mutable nil)
(define foods :mutable '())

(defun new-ground ()
  (let* [(body (love/physics/new-body world (/ 800 2) 775 "static"))
         (shape (love/physics/new-rectangle-shape 800 50))
         (fixture (love/physics/new-fixture body shape))]
   (self fixture :setUserData {:type :ground})
   { :body body :shape shape :fixture fixture }))

(defun set-dog-state (dog new-state)
  (.<! dog :state new-state)
  (.<! dog :anim (.> (.> dog :anims) new-state))
  (self (.> dog :anim) :resume))

(defun dog-advance-state (dog)
  (let [(next-state (case (.> dog :state)
                      [:closed :opening]
                      [:opening :open]
                      [:open :closing]
                      [:closing :closed]))]
  (set-dog-state dog next-state)))

(defun new-dog ()
  (let* [ (body (love/physics/new-body world 48 48 "kinematic"))
        (shape (love/physics/new-rectangle-shape 48 48))
        (fixture (love/physics/new-fixture body shape))
        (dog { :body body :shape shape :fixture fixture })
        (anims { :closed (new-animation (.> frames :dog-closed) 0.1 "pauseAtEnd")
               :opening (new-animation (.> frames :dog-opening)
                                       0.1
                                       (lambda () (dog-advance-state dog)))
               :open (new-animation (.> frames :dog-open) 0.1 "pauseAtEnd")
               :closing (new-animation (.> frames :dog-closing)
                                       0.1
                                       (lambda () (dog-advance-state dog)))})]
   (.<! dog :anims anims )
   (self fixture :setUserData :dog)
   (set-dog-state dog :closed)
   dog))

(defun draw-dog (dog)
  (self (.> dog :anim) :draw
        dog-sheet
        (self (.> dog :body) :getX)
        (self (.> dog :body) :getY)
        (self (.> dog :body) :getAngle)
        1
        1
        15
        30))

(defun new-food ()
  (let* [
    (body (love/physics/new-body world 600 600 "dynamic"))
    (shape (love/physics/new-circle-shape 32))
    (fixture (love/physics/new-fixture body shape))
    (anim (new-animation (.> frames :doughnut) 0.1))
    (sheet-key (sample (keys food-sheets)))]

    (self fixture :setDensity 0.5)
    (self body :applyLinearImpulse -30 -50)
    (self fixture :setUserData {:type :food :food-type sheet-key})
    (self body :setAngularVelocity 0.1)
    { :body body :shape shape :fixture fixture :anim anim :sheet-key sheet-key })
  )

; imports 4x sheet
(defun import-sheet (path)
  (let [(image (love/graphics/new-image path))]
    ; (self image :setFilter "nearest" "nearest")
    image))

; anim8 assets: sheets, grids n frames
(define food-sheets {
  :doughnut (import-sheet "assets/doughnut4x.png")
  :taco (import-sheet "assets/taco4x.png")
  :hotdog (import-sheet "assets/hotdog4x.png")
  :strawberry (import-sheet "assets/strawberry4x.png")
  :broccoli (import-sheet "assets/broccoli4x.png")
})

(define dog-sheet (import-sheet "assets/dog-e-dog4x.png"))

(define sounds {
  :throw (love/audio/new-source "assets/throw.wav" "static")
  })

(define grids {
  :doughnut (new-grid
             64
             64
             (self (.> food-sheets :doughnut) :getWidth)
             (self (.> food-sheets :doughnut) :getHeight))
  :dog (new-grid
        192
        192
        (self dog-sheet :getWidth)
        (self dog-sheet :getHeight))
  })

(define frames {
  :doughnut (self (.> grids :doughnut) :getFrames "1-4" 1)
  :dog-closed (self (.> grids :dog) :getFrames 1 1)
  :dog-opening (self (.> grids :dog) :getFrames "2-3" 1)
  :dog-open (self (.> grids :dog) :getFrames 3 1)
  :dog-closing (self (.> grids :dog) :getFrames "4-5" 1)
  })




; sets angle on a love.physics body, between the min and max angles (in radians)
(defun set-angle (body angle-delta min-angle max-angle)
  (let* [(old-angle (self body :getAngle))
        (new-angle (bounded (+ old-angle angle-delta)
                            min-angle
                            max-angle))]
   (when (/= old-angle new-angle)
     (self body :setAngle new-angle))))

(defun collision-with? (sym-a sym-b test-a test-b)
  (or (and (= sym-a test-a) (= sym-b test-b))
      (and (= sym-a test-b) (= sym-b test-a))))

; callback for collision detection
(defun begin-contact (a b coll)
  (let [(a-body (self a :getBody))
        (a-type (.> (self a :getUserData) :type))
        (b-body (self b :getBody))
        (b-type (.> (self b :getUserData) :type))]
    (when (collision-with? :food :ground a-type b-type)
      ;; set to destroy
      (self b-body :setUserData true))))

(defevent :load ()
  (love/window/set-mode 800 800 { :display 2 })
  (love/physics/set-meter 192)

  (set! world (love/physics/new-world 0 (* 9.81 192) true))
  (set! ground (new-ground))
  (set! dog (new-dog))

  ; callbacks?
  (self world :setCallbacks begin-contact nil nil nil)

  ; one-shot keys
  (.<! love :keypressed
       (lambda (key isRepeat)
         (cond
           [(= key "return")
            (dog-advance-state dog)]
           [(= key "space")
            (self (.> sounds :throw) :play)
            (set! foods (cons (new-food) foods))]
           [true]
           )))
)

(defevent :update (dt)
  (self world :update dt)
  (self (.> dog :anim) :update dt)

  ; update food list, removing and destroying objects marked for destruction
  (set! foods
        (reduce (lambda (foods food)
                  (self (.> food :anim) :update dt)
                  (case (self (.> food :body) :getUserData)
                    [nil (cons food foods)]
                    [true (self (.> food :body) :destroy) foods]))
                '()
                foods))

  ; keys
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

(defevent :draw ()
  (draw-dog dog)

  ; just draw all foods
  (do [(food foods)]
    (let* [(body (.> food :body))
           (anim (.> food :anim))
           (sheet (.> food-sheets (.> food :sheet-key)))
           (x (self body :getX))
           (y (self body :getY))]
      (self anim :draw sheet x y 0 1 1 32 32)))

  (love/graphics/set-color 0.5 0.8 0.3)
  (love/graphics/polygon
    "fill"
    (self (.> ground :body)
          :getWorldPoints
          (self (.> ground :shape) :getPoints)))

  ; must set color back to white at end of draw
  (love/graphics/set-color 1 1 1))
