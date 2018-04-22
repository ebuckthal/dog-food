(local anim8 (require "anim8"))
(local view (require "fennelview"))
(local scale 2)

(local sheets {
 "doughnut" (love.graphics.newImage "assets/doughnut.png")
 "dog" (love.graphics.newImage "assets/dog-e-dog.png")
})

(local grids {
 "doughnut" (anim8.newGrid 16 16 64 16)
 "dog" (anim8.newGrid 48 48 (: sheets.dog :getWidth) (: sheets.dog :getHeight))
})

(global dog
       {"body" nil
        "shape" nil
        "fixture" nil
        "state" "closed"
        "anim" (anim8.newAnimation (: grids.dog :getFrames "1-5" 1 1 1) 0.1)
        "paused" false})

(defn flip-dog-state []
  (let [new-state (if (= dog.state "closed") "open" "closed")]
    (tset dog "state" new-state)))

(var anim {})

(var canvas nil)
(var world nil)

(var balls [])

(var ground-body nil)
(var ground-shape nil)
(var ground-fixture nil)

(defn bounded [val ?min ?max]
  (if (and (~= ?max nil) (> val ?max)) ?max
      (and (~= ?min nil) (< val ?min)) ?min
      val))

;sets angle of a body
(defn set-angle [body angleDelta ?minAngle ?maxAngle]
  (let [oldAngle (: body :getAngle)
        newAngle (bounded (+ oldAngle angleDelta)
                          ?minAngle
                          ?maxAngle)]
    (when (~= oldAngle newAngle)
      (: body :setAngle newAngle))
    ))

(defn new-ball []
 (let [
   body (love.physics.newBody world 300 300 "dynamic")
   shape (love.physics.newCircleShape 8)
   fixture (love.physics.newFixture body shape)
   anim (anim8.newAnimation (: grids.doughnut :getFrames "1-4" 1) 0.1)
 ]
  (: body :applyLinearImpulse -8 -10)
  (: fixture :setUserData "ball")
  (: body :setAngularVelocity 0.1)
  { "body" body "shape" shape "fixture" fixture "anim" anim "sheet" sheets.doughnut}))

(defn beginContact [a b coll]
  (let [a-body (: a :getBody)
        a-data (: a :getUserData)
        b-body (: b :getBody)
        b-data (: b :getUserData)]
    (if
      (and (= "ball" b-data) (= "ground" a-data))
      (: b-body :setUserData true))))


(defn love.load []
  (love.window.setMode 800 800 { "display" 2 })
  (love.physics.setMeter 64)
  (set world (love.physics.newWorld 0 (* 9.81 64) true))
  (: world :setCallbacks beginContact nil nil nil)

  ; one shot keypresses defined here
  (tset love "keypressed"
        (fn [key isRepeat]
          (if (= key "return")
              (flip-dog-state))))

  (table.insert balls (new-ball))

  (set ground-body (love.physics.newBody world (/ 800 2) (/ 750 2) "static"))
  (set ground-shape (love.physics.newRectangleShape 800 50))
  (set ground-fixture (love.physics.newFixture ground-body ground-shape))
  (: ground-fixture :setUserData "ground")

  (tset dog "body" (love.physics.newBody world 100 200 "kinematic"))
  (tset dog "shape" (love.physics.newRectangleShape 100 50))
  (tset dog "fixture" (love.physics.newFixture dog.body dog.shape))

  (set canvas (love.graphics.newCanvas 800 800))
  (: canvas :setFilter "nearest" "nearest")

  ; (global sheet (love.graphics.newImage "sheet2.png"))
  ; (global g (anim8.newGrid 37 39 740 39))
  ; (global animation (anim8.newAnimation (: g :getFrames "1-20" 1) 0.1))

  (let [code (love.filesystem.read "stdio.fnl")
        data (love.filesystem.newFileData (fennel.compileString code) "io")
        thread (love.thread.newThread data)
        io-channel (love.thread.newChannel)]
    ;; this thread will send "eval" events for us to consume:
    (: thread :start "eval" io-channel)
    (set love.handlers.eval
         (fn [input]
           (let [(ok val) (pcall fennel.eval input)]
             (: io-channel :push (view val)))))))


(defn love.update [dt]
  (: world :update dt)

  ;(: animation :update dt)
  (if (= true dog.paused)
      (: dog.anim :pause)
      (: dog.anim :resume))

  (: dog.anim :update dt)

  (each [key value (pairs balls)]
    (: value.anim :update dt)
    (when (= true (: value.body :getUserData))
      (: value.body :destroy)
      ; is this cool? deletion in iteration
      (table.remove balls key)))

  (when (love.keyboard.isDown "space") (table.insert balls (new-ball)))
  (when (love.keyboard.isDown "w") (: dog.body :setY (+ (: dog.body :getY) -3)))
  (when (love.keyboard.isDown "a") (: dog.body :setX (+ (: dog.body :getX) -3)))
  (when (love.keyboard.isDown "s") (: dog.body :setY (+ (: dog.body :getY) 3)))
  (when (love.keyboard.isDown "d") (: dog.body :setX (+ (: dog.body :getX) 3)))
  (when (love.keyboard.isDown "q") (set-angle dog.body -0.1 -1 1))
  (when (love.keyboard.isDown "e") (set-angle dog.body 0.1 -1 1)))

(defn love.draw []
  (love.graphics.setCanvas canvas)
  (love.graphics.clear)

  (if (= dog.state "closed") (: dog.anim :gotoFrame 1)
      (= dog.state "open") (: dog.anim :gotoFrame 3))

  (: dog.anim :draw sheets.dog
     (: dog.body :getX)
     (: dog.body :getY)
     (: dog.body :getAngle)
     1
     1
     15
     30)

  (love.graphics.setColor 0.5 0.8 0.3)
  (love.graphics.polygon "fill" (: ground-body :getWorldPoints (: ground-shape :getPoints)))
  (love.graphics.setColor 1 1 1)

  (each [key value (pairs balls)]
    (: value.anim :draw value.sheet
      (: value.body :getX)
      (: value.body :getY)
      (: value.body :getAngle)
      1
      1
      8
      8))

  ; (: animation :draw sheet (: ball-body :getX) (: ball-body :getY))

  (love.graphics.setCanvas)
  (love.graphics.draw canvas 0 0 0 scale scale)
)
