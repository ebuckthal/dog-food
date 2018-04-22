(local anim8 (require "anim8"))
(local view (require "fennelview"))
(local scale 2)

(var canvas nil)
(var world nil)

(var balls [])

(var ground-body nil)
(var ground-shape nil)
(var ground-fixture nil)

(var dog-body nil)
(var dog-shape nil)
(var dog-fixture nil)

(defn set-angle [body angleDelta]
  (: body :setAngle (+ (: body :getAngle) angleDelta)))

(defn new-ball []
 (let [
   body (love.physics.newBody world 300 300 "dynamic")
   shape (love.physics.newCircleShape 20)
   fixture (love.physics.newFixture body shape)
 ]
  (: body :applyLinearImpulse -80 -100)
  (: fixture :setUserData "ball")
  { "body" body "shape" shape "fixture" fixture }))

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

  (table.insert balls (new-ball))

  (set ground-body (love.physics.newBody world (/ 800 2) (/ 750 2) "static"))
  (set ground-shape (love.physics.newRectangleShape 800 50))
  (set ground-fixture (love.physics.newFixture ground-body ground-shape))
  (: ground-fixture :setUserData "ground")

  (set dog-body (love.physics.newBody world 100 200 "kinematic"))
  (set dog-shape (love.physics.newRectangleShape 100 50))
  (set dog-fixture (love.physics.newFixture dog-body dog-shape))

  (set canvas (love.graphics.newCanvas 800 800))
  (: canvas :setFilter "nearest" "nearest")
  (global sheet (love.graphics.newImage "sheet2.png"))
  (global g (anim8.newGrid 37 39 740 39))
  (global animation (anim8.newAnimation (: g :getFrames "1-20" 1) 0.1))
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
  (: animation :update dt)

  (each [key value (pairs balls)]
    (when (= true (: value.body :getUserData))
      (: value.body :destroy)
      ; is this cool? deletion in iteration
      (table.remove balls key)))

  (when (love.keyboard.isDown "space") (table.insert balls (new-ball)))
  (when (love.keyboard.isDown "w") (: dog-body :setY (+ (: dog-body :getY) -3)))
  (when (love.keyboard.isDown "a") (: dog-body :setX (+ (: dog-body :getX) -3)))
  (when (love.keyboard.isDown "s") (: dog-body :setY (+ (: dog-body :getY) 3)))
  (when (love.keyboard.isDown "d") (: dog-body :setX (+ (: dog-body :getX) 3)))
  (when (love.keyboard.isDown "q") (set-angle dog-body -0.1))
  (when (love.keyboard.isDown "e") (set-angle dog-body 0.1)))

(defn love.draw []
  (love.graphics.setCanvas canvas)
  (love.graphics.clear)

  (love.graphics.setColor 0.5 0.8 0.3)

  (love.graphics.polygon
   "fill"
   (: dog-body :getWorldPoints (: dog-shape :getPoints)))

  (each [key value (pairs balls)]
    (love.graphics.circle
      "fill"
      (: value.body :getX)
      (: value.body :getY)
      (: value.shape :getRadius)))

  (love.graphics.polygon "fill" (: ground-body :getWorldPoints (: ground-shape :getPoints)))

  ; (: animation :draw sheet (: ball-body :getX) (: ball-body :getY))

  (love.graphics.setCanvas)
  (love.graphics.draw canvas 0 0 0 scale scale)
)
