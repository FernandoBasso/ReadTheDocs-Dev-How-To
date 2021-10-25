(import test)
;(load "find-n-v1.scm")
;(load "find-n-v2.scm")
(load "find-n-v3.scm")

(test-group
 "find-n"
 (test-group
  "when input is an empty list"
  (test "should find nothing"
        #f
        (find-n '())))
 (test-group
  "when input contains a single integer"
  (test "should return that number"
        7
        (find-n '(7)))))
