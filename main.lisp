(import love/love (defevent))
(import love/graphics)
(import love/physics)
(import love/image)
(import love/window)
(import love/keyboard)
(import love/audio)
(import love/timer ())

; import from helpers.lisp
(import helpers ())
(import timer timer)

(define love :hidden (require "love"))

(define anim8 :hidden (require "anim8"))
(define new-animation (.> anim8 :newAnimation))
(define new-grid (.> anim8 :newGrid))

(define flux :hidden (require "flux"))
(define flux/update (.> flux :update))
(define flux/to (.> flux :to))

(define fonts '())
(define font-index :mutable 1)
(define last-font-time :mutable (get-time))

(defun get-x (body) (self body :getX))
(defun get-y (body) (self body :getY))

(define dog-home-x 192)
(define dog-home-y 600)

; game objects
(define scene :mutable "title")

(define score :mutable 0)

(define world :mutable nil)
(define canvas :mutable nil)
(define dog :mutable nil)
(define foods :mutable '())

(define title-menu-options '("new game" "how to play" "credits"))
(define title-menu-index :mutable 1)


(defun title-menu-next-index (delta)
  (let [(next-index (+ delta title-menu-index))]
    (cond
      [(> next-index (n title-menu-options)) title-menu-index]
      [(< next-index 1) title-menu-index]
      [true next-index])
    ))

(defun new-ground ()
  (let* [(body (love/physics/new-body world (/ 800 2) 775 "static"))
         (shape (love/physics/new-rectangle-shape 800 25))
         (fixture (love/physics/new-fixture body shape))]
   (self fixture :setUserData {:type :ground})
   (self fixture :setFriction 0.9)
   { :body body :shape shape :fixture fixture }))

(defun new-left-wall ()
  (let* [(body (love/physics/new-body world -100 (/ 800 2) "static"))
         (shape (love/physics/new-rectangle-shape 50 800))
         (fixture (love/physics/new-fixture body shape))]
   (self fixture :setFriction 0.9)
   { :body body :shape shape :fixture fixture }))

(defun new-right-wall ()
  (let* [(body (love/physics/new-body world -100 (/ 800 2) "static"))
         (shape (love/physics/new-rectangle-shape 50 800))
         (fixture (love/physics/new-fixture body shape))]
   (self fixture :setFriction 0.9)
   { :body body :shape shape :fixture fixture }))

(defun dog-open-mouth-stuff (dog)
  (self (.> dog :dog-bottom-face-fixture) :setSensor true))

(defun dog-closed-mouth-stuff (dog)
  (self (.> dog :dog-bottom-face-fixture) :setSensor false))

(defun set-dog-state (dog new-state)
  (let [(cur-state (.> dog :state))]
    (.<! dog :state new-state)
    (.<! dog :anim (.> (.> dog :anims) new-state))
    (self (.> dog :anim) :resume)
    (when (and (= cur-state :open) (dog-has-food? dog))
      (dog-eat-food dog)
      (self (.> sounds :eat) :play))
    (when (= cur-state :open)
      (dog-open-mouth-stuff dog))
    (when (= cur-state :closed)
      (dog-closed-mouth-stuff dog))))



(defun dog-advance-state (dog)
  (let* [(cur-state (.> dog :state))
         (next-state (case cur-state
                       [:closed :opening]
                       [:opening :open]
                       [:open :closing]
                       [:closing :closed]))]
    (set-dog-state dog next-state)))

(defun dog-maybe-catch-food (dog food-fixture)
  (when (and (= :open (.> dog :state)) (not (dog-has-food? dog)))
    (dog-catch-food dog food-fixture)))

(defun dog-has-food? (dog) (not (= nil (.> dog :has-food-type))))

(defun dog-catch-food (dog food-fixture)
  (.<! dog :has-food-type (.> (self food-fixture :getUserData) :food-type))
  (fixture-tell-body-to-die food-fixture)
  )

(defun dog-eat-food (dog)
  (set! score (+ score 1))
  (.<! dog :has-food-type nil))

(defun new-fixture (body fixture-data is-sensor shape)
  (let [(fixture (love/physics/new-fixture body shape))]
    (self fixture :setUserData fixture-data)
    (self fixture :setSensor is-sensor)
    fixture))

;; turns a list of point lists into a list of shifted points (not a list of shifted list points mind you)
(defun shift-vertices (x y point-alist)
  (flat-map (lambda (lst) (list (+ x (car lst)) (+ y (cadr lst))))
            point-alist))

(defun new-dog ()
  (let* [(body (love/physics/new-body world dog-home-x dog-home-y "dynamic"))
         (dog-bottom-face-fixture
          (new-fixture body
                       {:type :dog}
                       false
                       (apply love/physics/new-polygon-shape
                              (shift-vertices -40 -60
                                              '((0 62)
                                                (86 62)
                                                (138 86)
                                                (0 92))))))
         (dog {:body body
               :state nil
               :anim nil
               :anims nil
               :dog-bottom-face-fixture dog-bottom-face-fixture
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
    (self body :setAngularDamping 8) ; stop spinning one day
    (self body :setLinearDamping 2)
    (new-fixture
           body
           {:type :dog}
           false
           (apply love/physics/new-polygon-shape
                  (shift-vertices -40 -60
                                  '((0 0)
                                    (68 0)
                                    (148 8)
                                    (148 26)
                                    (86 62)
                                    (0 62)))))
    (new-fixture body
               {:type :dog-mouth}
               true
               (love/physics/new-rectangle-shape 80 10 50 20 0.1))
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

(defun spawn-food ()
  (set! foods (cons (new-food) foods)))

(defun new-food ()
  (let* [
    (init-x (random-range 500 700))
    (init-y (random-range 500 700))
    (init-impulse-x (random-range -50 -40))
    (init-impulse-y (random-range -100 -80))
    (body (love/physics/new-body world init-x init-y "dynamic"))
    ; (shape (love/physics/new-circle-shape 32))
    (shape (apply
             love/physics/new-polygon-shape
             (shift-vertices 0 -22
              '((-30 0) (0 45) (30 0)))))
    (fixture (love/physics/new-fixture body shape))
    (anim (new-animation (.> frames :doughnut) 0.15))
    (sheet-key (sample (keys food-sheets)))]

    (self fixture :setDensity 3)
    (self body :resetMassData)
    (self fixture :setFriction 0.9)
    (self body :applyLinearImpulse init-impulse-x init-impulse-y)
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
  :eat (love/audio/new-source "assets/eat.wav" "static")
  :splat (love/audio/new-source "assets/splat.wav" "static")
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

(defun find-food-by-fixture (fixture)
  (let [(index (find-index (lambda (food) (= (.> food :fixture) fixture)) foods))]
    (if (nil? index)
      nil
      (nth foods index))))

; callback for collision detection
(defun begin-contact (a b coll)
  (collision-with
   :food :dog-mouth a b
   (lambda (food-fixture _) (dog-maybe-catch-food dog food-fixture)))
  ; (collision-with
  ;  :food :ground a b
  ;  (lambda (food-fixture _) (fixture-tell-body-to-die food-fixture)))
  (collision-with
   :food :dog a b
   (lambda () (self (.> sounds :splat) :play))))

(defun on-keypress (key is-repeat)
  (when (= scene "title")
    (when (= key "down")
      (set! title-menu-index (title-menu-next-index 1)))
    (when (= key "up")
      (set! title-menu-index (title-menu-next-index -1)))
    (when (= key "return")
      (when (= (nth title-menu-options title-menu-index) "new game")
        (set! scene "game"))
      (when (= (nth title-menu-options title-menu-index) "how to play")
        (set! scene "how-to"))
      (when (= (nth title-menu-options title-menu-index) "credits")
        (set! scene "credits"))))

  (when (= scene "game")
    (when (= key "escape")
      (set! scene "title"))
    (when (= key "return")
      (dog-advance-state dog))
    (when (= key "space")
      ; (self (.> sounds :throw) :play)
      (spawn-food))
    (when (= key "a")
      (body-impulse-vector (.> dog :body) { :x -60 :y 0 }))
    (when (= key "d")
      (body-impulse-vector (.> dog :body) { :x 60 :y 0 }))
    (when (= key "w")
      (body-impulse-vector (.> dog :body) { :x 0 :y -60 }))
    (when (= key "s")
      (body-impulse-vector (.> dog :body) { :x 0 :y 60 })))

  (when (= scene "how-to")
    (when (= key "escape")
      (set! scene "title")))

  (when (= scene "credits")
    (when (= key "escape")
      (set! scene "title")))
  )

(defmacro make-tween (name next obj time newobj)
  `(defun ,name ()
     (self (self (flux/to ,obj ,time ,newobj)
                 :oncomplete ,next)
           :ease "sineinout")))

(define sun-position {:x 300 :y 0})
(make-tween st1 st2
            sun-position
            3
            {:x 320 :y -10})
(make-tween st2 st3
            sun-position
            3
            {:x 300 :y -10})
(make-tween st3 st1
            sun-position
            3
            {:x 300 :y 0})

(define close-grass-position {:x -50 :y 400})
(make-tween close-grass-1 close-grass-2
            close-grass-position
            3
            {:x -45})
(make-tween close-grass-2 close-grass-1
            close-grass-position
            3
            {:x -50})

(define mid-grass-position {:x -50 :y 400})
(make-tween mid-grass-1 mid-grass-2
            mid-grass-position
            3
            {:x -53})
(make-tween mid-grass-2 mid-grass-1
            mid-grass-position
            3
            {:x -50})

(define far-grass-position {:x -50 :y 400})
(make-tween far-grass-1 far-grass-2
            far-grass-position
            3
            {:x -48})
(make-tween far-grass-2 far-grass-1
            far-grass-position
            3
            {:x -50})

(defevent :load ()
  (push! fonts (love/graphics/new-image-font "assets/test-font1.png" " abcdefghijklmnopqrstuvwxyz0123456789.,'!?"))
  (push! fonts (love/graphics/new-image-font "assets/test-font2.png" " abcdefghijklmnopqrstuvwxyz0123456789.,'!?"))

  (love/window/set-mode 800 800 { :display 2 })
  (love/physics/set-meter 192)

  (set! world (love/physics/new-world 0 (* 9.81 192) true))
  (new-left-wall)
  (new-ground)
  (new-right-wall)
  (set! dog (new-dog))

  ;; start some tweens
  (st1)
  (close-grass-1)
  (mid-grass-1)
  (far-grass-1)

  ; callbacks?
  (self world :setCallbacks begin-contact nil nil nil)

  ; one-shot keys
  (.<! love :keypressed on-keypress)

  ; setup callback for food spawning
  (timer/on-percent-chance 0.3 0.3 (lambda () (spawn-food)))
)

(define time-last-food :mutable nil)
(define time-delta-food 1)


(defun get-speed (body)
  (velocity-to-speed (pself body :getLinearVelocity)))

(defevent :update (dt)
  (when (= scene "title"))

  (when (= scene "game")
    (flux/update dt)
    (self world :update dt)
    (self (.> dog :anim) :update dt)

    (timer/update dt)

    (let [(v (scale-vector (body-vector-to (.> dog :body) dog-home-x dog-home-y) 3))]
      (self (.> dog :body) :applyForce (- 0 (.> v :x)) (- 0 (.> v :y))))

    ; update food list, destroy if marked for destruction
    ; pause animation if moving too slowly
    (do [(food foods)]
      (let* [(body (.> food :body))
            (anim (.> food :anim))
            (sheet (.> food-sheets (.> food :sheet-key)))]
        (self anim :update dt)
        (when (not (self body :isDestroyed))
          (when (< (get-speed body) 100)
            (self anim :pause))
          (when (self body :getUserData)
            (self body :destroy)))
        ))

    (when (love/keyboard/is-down "q")
      (set-angle (.> dog :body) -0.1 -1 1))
    (when (love/keyboard/is-down "e")
      (set-angle (.> dog :body) 0.1 -1 1)))

  )

(define bg-sky (love/graphics/new-image "assets/sky.png"))
(define bg-sun (love/graphics/new-image "assets/sun.png"))
(define close-grass (love/graphics/new-image "assets/close-grass.png"))
(define mid-grass (love/graphics/new-image "assets/mid-grass.png"))
(define far-grass (love/graphics/new-image "assets/far-grass.png"))

(defun draw-sun ()
  (love/graphics/set-color 1 1 1 1)
  (love/graphics/draw bg-sun (.> sun-position :x) (.> sun-position :y)))

(defun draw-bg ()
  (love/graphics/draw bg-sky 0 0)

  (love/graphics/set-color 0.2 0.2 1 0.2)
  (love/graphics/rectangle "fill" 0 0 800 800)

  (draw-sun)

  (love/graphics/set-color 0.2 0.2 1 0.1)
  (love/graphics/rectangle "fill" 0 0 800 800)
  (love/graphics/set-color 1 1 1 1)
  (love/graphics/draw far-grass
                      (.> far-grass-position :x)
                      (.> far-grass-position :y))
  (love/graphics/draw mid-grass
                      (.> mid-grass-position :x)
                      (.> mid-grass-position :y)))

(defun draw-fg ()
  (love/graphics/set-color 1 1 1 1)
  (love/graphics/draw close-grass
                      (.> close-grass-position :x)
                      (.> close-grass-position :y)))

(defun draw-ui ()
  (love/graphics/set-color 0 0 0 0.2)
  (love/graphics/print score 22 22)
  (love/graphics/set-color 1 1 1 1)
  (love/graphics/print score 20 20)
  )

(defun draw-menu ()
  (do ([option title-menu-options])
    (let [(index (element-index option title-menu-options))]

      ; draw a little shadow
      (love/graphics/set-color 0 0 0 0.2)
      (love/graphics/print option 252 (+ 402 (* 100 index)))

      ; set color red if selected
      (if (= title-menu-index index)
        (love/graphics/set-color 1 0 0 1)
        (love/graphics/set-color 1 1 1 1))

      (love/graphics/print option 250 (+ 400 (* 100 index)))
      )

    ; reset color for others
    (love/graphics/set-color 1 1 1 1)))

(defevent :draw ()
  (when (> (get-time) (+ 0.1 last-font-time))
    (set! font-index (if (= 1 font-index) 2 1))
    (set! last-font-time (get-time))
    (love/graphics/set-font (nth fonts font-index) 24))

  (when (= scene "title")
    (draw-bg)
    (love/graphics/print "dog eat food world" 20 20)
    (draw-menu))

  (when (= scene "how-to")
    (draw-bg)
    (love/graphics/print "eat all the food!" 40 240)
    (love/graphics/print "enter to open mouth!" 40 320)
    (love/graphics/print "enter again to eat!" 40 400)
    (love/graphics/print "q or e to tilt head!" 40 480))

  (when (= scene "credits")
    (draw-bg)
    (love/graphics/print "credits here!" 40 240))

  (when (= scene "game")
    (draw-bg)
    (draw-dog dog)
    ;;(draw-shapes (.> dog :body))


    ; just draw all foods
    (do [(food foods)]
      (let* [(body (.> food :body))
            (anim (.> food :anim))
            (sheet (.> food-sheets (.> food :sheet-key))) ]
        (when (not (self body :isDestroyed))
          (self anim :draw
                sheet (get-x body) (get-y body) 0 1 1 32 32))))


    ; must set color back to white at end of draw
    (love/graphics/set-color 1 1 1)
    (draw-fg)
    (draw-ui))
)
