(import test)
(load "find-n-v3.scm")

(test-group
 "count-occurrences"
 (test 0 (count-occurrences 7 '()))
 (test 1 (count-occurrences 7 '(7)))
 (test 2 (count-occurrences 7 '(7 -7 7 1)))
 (test 5 (count-occurrences 7 '(7 1 7 2 7 7 3 7))))

