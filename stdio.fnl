(require "love.event")

;; This module exists in order to expose stdio over a channel so that it
;; can be used in a non-blocking way from another thread.

(local (event channel) ...)
(let [prompt (fn [] (io.write "> ") (io.flush) (io.read "*l"))]
  ((fn looper [input]
     (when input
       ;; This is consumed by love.handlers.eval which is defined in love.load.
       (love.event.push event input)
       (print (: channel :demand))
       (looper (prompt)))) (prompt)))
