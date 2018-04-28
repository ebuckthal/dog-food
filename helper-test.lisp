(import lua/math (floor))
(import helpers (distance))

(import test ())


(describe "tests actually work"
  (it "should succeed"
      (affirm (= 1 1))))


(describe "distance"
  (it "does an easy test"
    (affirm (= (distance 0 0 0 1) 1)))
  (it "does positive numbers"
    (affirm (= (floor (distance 2 3 10 77)) 74))))
