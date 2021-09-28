(import test)
(load "find-n-v2.scm")

(test-group
 "eqx?"
 (test-group
  "should assert that two numbers are equal"
  (test #t ((eqx? 1) 1))
  (test #t ((eqx? 42) (+ 41 1))))
 (test-group
  "should that two numbers are different"
  (test #f ((eqx? 1) -1))
  (test #f ((eqx? 0) (- 0 1)))))

(test-group
 "oddx?"
 (test-group
  "should return #t when input list contains odd number of x"
  (test #t (oddx? 42 '(42)))
  (test #t (oddx? 42 '(0 -42 42 1 42 -7 42))))
 (test-group
  "should return #f when input list contains even number of x"
  (test #f (oddx? 42 '()))
  (test #f (oddx? 42 '(-42)))
  (test #f (oddx? 42 '(0 -42 42 1 -7 42)))))

