(import lua/math ())

(define timer :mutable nil)
(define timer-percent :mutable nil)
(define timer-interval :mutable nil)
(define timer-last-interval :mutable nil)
(define timer-start :mutable nil)
(define timer-fn :mutable nil)
(define timer-last-recur :mutable nil)
(define current-time :mutable 0)

(defun update (dt)
  (set! current-time (+ current-time dt))
  (when (> current-time
           (+ timer-interval timer-last-interval))
    (set! timer-last-interval current-time)
    (print! "maybe thing will happen?")
    (when (< (random) timer-percent)
      ; call the callback
      (timer-fn timer-last-recur)
      (set! timer-last-recur current-time)))
  )

(defun on-percent-chance (percent interval fn)
  (print! "registered")
  (set! timer-start current-time)
  (set! timer-last-recur current-time)
  (set! timer-last-interval current-time)
  (set! timer-percent percent)
  (set! timer-interval interval)
  (set! timer-fn fn))
