(import love/love (defevent))
(import love/graphics)
(import love/physics)
(import love/image)
(import love/window)
(import love/keyboard)
(import love/audio)


; import from helpers.lisp
(import helpers ())

(define love :hidden (require "love"))
(define anim8 :hidden (require "anim8"))
(define new-animation (.> anim8 :newAnimation))
(define new-grid (.> anim8 :newGrid))

(defun get-x (body) (self body :getX))
(defun get-y (body) (self body :getY))

(define dog-home-x 192)
(define dog-home-y 192)

; game objects

(define score :mutable 0)

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
  (let* [(cur-state (.> dog :state))
         (next-state (case cur-state
                       [:closed :opening]
                       [:opening :open]
                       [:open :closing]
                       [:closing :closed]))]
    (when (and (= cur-state :open) (dog-has-food? dog))
      (dog-eat-food dog))
    (set-dog-state dog next-state)))

(defun dog-maybe-catch-food (dog food-fixture)
  (when (and (= :open (.> dog :state)) (not (dog-has-food? dog)))
    (dog-catch-food dog food-fixture)))

(defun dog-has-food? (dog) (not (= nil (.> dog :has-food-type))))

(defun dog-catch-food (dog food-fixture)
  (print! "dog caught food")
  (.<! dog :has-food-type (.> (self food-fixture :getUserData) :food-type)))

(defun dog-eat-food (dog)
  (set! score (+ score 1))
  (.<! dog :has-food-type nil))

(defun new-shape (body fixture-data shape)
  (let [(fixture (love/physics/new-fixture body shape))]
    (self fixture :setUserData fixture-data)
    shape))

;; turns a list of point lists into a list of shifted points (not a list of shifted list points mind you)
(defun shift-vertices (x y point-alist)
  (flat-map (lambda (lst) (list (+ x (car lst)) (+ y (cadr lst))))
            point-alist))

(defun new-dog ()
  (let* [(body (love/physics/new-body world dog-home-x dog-home-y "dynamic"))
         (dog {:body body
               :state nil
               :anim nil
               :anims nil
               :dog-sprite-origin '(-60 -95)
               :food-sprite-origin '(70 -20)
               :has-food-type nil})
         (anims {:closed (new-animation (.> frames :dog-closed) 0.1 "pauseAtEnd")
                 :opening (new-animation (.> frames :dog-opening)
                                         0.1
                                         (lambda () (dog-advance-state dog)))
                 :open (new-animation (.> frames :dog-open) 0.1 "pauseAtEnd")
                 :closing (new-animation (.> frames :dog-closing)
                                         0.1
                                         (lambda () (dog-advance-state dog)))})]
    (.<! dog :anims anims)
    (self body :setGravityScale 0) ; no gravity on the dog plz
    (self body :setAngularDamping 2) ; stop spinning one day
    (self body :setLinearDamping 2)
    (new-shape
           body
           {:type :dog}
           (apply love/physics/new-polygon-shape
                  (shift-vertices -40 -60
                                  '((0 0)
                                    (68 0)
                                    (148 8)
                                    (148 26)
                                    (86 62)
                                    (0 62)))))
    (new-shape body
               {:type :dog}
               (apply love/physics/new-polygon-shape
                      (shift-vertices -40 -60
                                      '((0 62)
                                        (86 62)
                                        (138 86)
                                        (0 92)))))
    (new-shape body
               {:type :dog-mouth}
               (love/physics/new-rectangle-shape 118 102 50 20 0.1))
    (set-dog-state dog :closed)
    dog))

(defun draw-on-body (body animation sheet local-point)
  (apply self animation :draw
         sheet
         `(,@(pself body :getWorldPoint (nth local-point 1) (nth local-point 2))
           ,(self body :getAngle)
           1 1)))

(defun draw-dog (dog)
  (when (dog-has-food? dog)
    (with (food (.> dog :has-food-type))
      (draw-on-body (.> dog :body)
                    (.> food-stills food)
                    (.> food-sheets food)
                    (.> dog :food-sprite-origin))))
  (draw-on-body (.> dog :body)
                (.> dog :anim)
                dog-sheet
                (.> dog :dog-sprite-origin)))

(defun new-food ()
  (let* [
    (body (love/physics/new-body world 600 600 "dynamic"))
    (shape (love/physics/new-circle-shape 32))
    (fixture (love/physics/new-fixture body shape))
    (anim (new-animation (.> frames :doughnut) 0.15))
    (sheet-key (sample (keys food-sheets)))]

    (self fixture :setDensity 0.5)
    (self body :applyLinearImpulse -30 -80)
    (self fixture :setUserData {:type :food :food-type sheet-key})
    (self body :setAngularVelocity 0.1)
    { :body body :shape shape :fixture fixture :anim anim :sheet-key sheet-key })
  )


(defun draw-shapes (body)
  (do [(shape (map (lambda (fixture) (self fixture :getShape))
                   (values (self body :getFixtures))))]
      (love/graphics/set-color 1 0 0 0.2)
      (case (self shape :getType)
        ["polygon"
         (love/graphics/polygon
                    "fill"
                    (self body :getWorldPoints (self shape :getPoints)))]
        ["circle"
         (apply love/graphics/circle
                   "fill"
                   `(,@(pself body :getWorldPoints (self shape :getPoint))
                     ,(self shape :getRadius)))]
        [true (print! "no match, hmm...")])
      (love/graphics/set-color 1 1 1)))

;; this function must return a function that can be called
;; pcall(shape:getPoint)
;; pcall(shape.getPoint, shape)
(defun partial (fn &args)
  (lambda (&moreargs) (apply fn (append args moreargs))))

(defun my-get-point (shape)
  (partial (.> shape :getPoint) shape))

(defun draw-shapes-tests (body)
  (let* [(world (if body nil (love/physics/new-world 0 (* 9.81 192) true)))
         (body (if body body (love/physics/new-body world 100 100 "static")))
         (rect-shape (love/physics/new-rectangle-shape 100 100 500 500))
         (rect-fixture (love/physics/new-fixture body rect-shape))
         (poly-shape (love/physics/new-polygon-shape 0 0 100 100 0 100))
         (poly-fixture (love/physics/new-fixture body poly-shape))
         (circle-shape (love/physics/new-circle-shape 10 1 500))
         (circle-fixture (love/physics/new-fixture body circle-shape))]
    ;(debug (cdr (list (pcall (.> circle-shape :getPoint) circle-shape))))
;;                        (self circle-shape :getPoint))))
    ;;(affirm (= true (fixture-is-shape? rect-fixture "polygon")))
    ;;(affirm (= false (fixture-is-shape? rect-fixture "circle")))
    ;;(affirm (= true (fixture-is-shape? circle-fixture "circle")))
    ;;(affirm (= false (fixture-is-shape? circle-fixture "polygon")))
    (draw-shapes body)
    (if world
      (self world :destroy)
      (progn
       (self rect-fixture :destroy)
       (self circle-fixture :destroy)
       (self poly-fixture :destroy)))))

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
  ;; assuming all food is 16x16 at 4x with 4 frames
  :food (new-grid
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
  :doughnut (self (.> grids :food) :getFrames "1-4" 1)
  :dog-closed (self (.> grids :dog) :getFrames 1 1)
  :dog-opening (self (.> grids :dog) :getFrames "2-3" 1)
  :dog-open (self (.> grids :dog) :getFrames 3 1)
  :dog-closing (self (.> grids :dog) :getFrames "4-5" 1)
  })

(define food-stills
  {:doughnut (new-animation (self (.> grids :food) :getFrames 1 1) 0.1 "pauseAtEnd")
   :taco (new-animation (self (.> grids :food) :getFrames 1 1) 0.1 "pauseAtEnd")
   :hotdog (new-animation (self (.> grids :food) :getFrames 1 1) 0.1 "pauseAtEnd")
   :strawberry (new-animation (self (.> grids :food) :getFrames 1 1) 0.1 "pauseAtEnd")
   :broccoli (new-animation (self (.> grids :food) :getFrames 1 1) 0.1 "pauseAtEnd")
   })

; sets angle on a love.physics body, between the min and max angles (in radians)
(defun set-angle (body angle-delta min-angle max-angle)
  (let* [(old-angle (self body :getAngle))
        (new-angle (bounded (+ old-angle angle-delta)
                            min-angle
                            max-angle))]
   (when (/= old-angle new-angle)
     (self body :setAngle new-angle))))

(defun collision-with (type-a type-b fixture-a fixture-b fn)
  (let [(data-a (.> (self fixture-a :getUserData)))
        (data-b (.> (self fixture-b :getUserData)))]
    (when (and (not (nil? data-a)) (not (nil? data-b)))
      (let [(test-a (.> (self fixture-a :getUserData) :type))
            (test-b (.> (self fixture-b :getUserData) :type))]
        (cond [(and (= type-a test-a) (= type-b test-b))
               (fn fixture-a fixture-b)]
              [(and (= type-a test-b) (= type-b test-a))
               (fn fixture-b fixture-a)]
              [true nil])))))

(defun fixture-tell-body-to-die (fixture)
  (self (self fixture :getBody) :setUserData true))


(defun body-distance-to (body x y)
  (distance (self body :getX) (self body :getY) x y))

(defun body-vector-to (body x y)
  (vector-to (self body :getX) (self body :getY) x y))

(defun body-impulse-vector (body v)
  (self body :applyLinearImpulse (.> v :x) (.> v :y)))

(defun body-force-vector (body v)
  (self body :applyForce (.> v :x) (.> v :y)))

; callback for collision detection
(defun begin-contact (a b coll)
  (collision-with
   :food :dog-mouth a b
   (lambda (food-fixture _) (dog-maybe-catch-food dog food-fixture)))
  (collision-with
   :food :ground a b
   (lambda (food-fixture _) (fixture-tell-body-to-die food-fixture))))

(defevent :load ()

  (love/graphics/set-new-font "assets/Hack-Regular.ttf" 36)

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
            ; (self (.> sounds :throw) :play)
            (set! foods (cons (new-food) foods))]
           [(= key "a")
            (body-impulse-vector (.> dog :body) { :x -30 :y 0 })]
           [(= key "d")
            (body-impulse-vector (.> dog :body) { :x 30 :y 0 })]
           [(= key "w")
            (body-impulse-vector (.> dog :body) { :x 0 :y -30 })]
           [(= key "s")
            (body-impulse-vector (.> dog :body) { :x 0 :y 30 })]
           [true]
           )))
)

(defevent :update (dt)
  (self world :update dt)
  (self (.> dog :anim) :update dt)

  (let [(v (body-vector-to (.> dog :body) dog-home-x dog-home-y))]
    (self (.> dog :body) :applyForce (- 0 (.> v :x)) (- 0 (.> v :y))))

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
  ; (when (love/keyboard/is-down "w")
  ;   (self (.> dog :body) :setY (- (self (.> dog :body) :getY) 3)))
  ; (when (love/keyboard/is-down "a")
  ;   (self (.> dog :body) :setX (- (self (.> dog :body) :getX) 3)))
  ; (when (love/keyboard/is-down "s")
  ;   (self (.> dog :body) :setY (+ (self (.> dog :body) :getY) 3)))
  ; (when (love/keyboard/is-down "d")
  ; (self (.> dog :body) :setX (+ (self (.> dog :body) :getX) 3)))
  (when (love/keyboard/is-down "q")
    (set-angle (.> dog :body) -0.1 -1 1))
  (when (love/keyboard/is-down "e")
    (set-angle (.> dog :body) 0.1 -1 1)))

(defun draw-ui ()
  (love/graphics/print score 20 20))

(defevent :draw ()
  (draw-dog dog)
  (draw-shapes (.> dog :body))


  ; just draw all foods
  (do [(food foods)]
    (let* [(body (.> food :body))
           (anim (.> food :anim))
           (sheet (.> food-sheets (.> food :sheet-key)))
           (x (self body :getX))
           (y (self body :getY))]
      (self anim :draw sheet x y 0 1 1 32 32)))

  (love/graphics/set-color 1 0 0 0.2)
  (love/graphics/circle "fill" dog-home-x dog-home-y 100)

  (love/graphics/set-color 0.5 0.8 0.3)
  (love/graphics/polygon
    "fill"
    (self (.> ground :body)
          :getWorldPoints
          (self (.> ground :shape) :getPoints)))

  ; must set color back to white at end of draw
  (love/graphics/set-color 1 1 1)
  (draw-ui))
