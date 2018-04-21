(local anim8 (require "anim8"))
(local view (require "fennelview"))
(local scale 2)

(var canvas nil)

(defn love.load []
  (set canvas (love.graphics.newCanvas 800 600))
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
  (: animation :update dt)
)

(defn love.draw []
  (love.graphics.setCanvas canvas)
  (love.graphics.clear)
  (love.graphics.setColor 1 1 1)

  (: animation :draw sheet 0 0)

  (love.graphics.setCanvas)
  (love.graphics.draw canvas 0 0 0 scale scale)
)
