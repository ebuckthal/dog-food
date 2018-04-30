(import love/graphics)
(import love/physics)

(import helpers ())

; helper for drawing all shapes of a body
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
