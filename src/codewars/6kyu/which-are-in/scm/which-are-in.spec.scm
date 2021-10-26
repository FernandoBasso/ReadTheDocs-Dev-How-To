(import test)
(load "which-are-in-v1.scm")

(define haystack '("lively" "alive" "harp" "sharp" "armstrong"))

(test-group
 "in-list"
 (test-group
  "when substrings is empty"
  (test "should find nothing"
        '()
        (in-list '() haystack)))

 (test-group
  "when strings is empty"
  (test "should find nothing"
        '()
        (in-list '("tarp" "mice" "bull") '())))

 (test-group
  "when there is one match"
  (test "should return a list with one match"
        '("live")
        (in-list '("xyz" "live" "force") haystack)))

 (test-group
  "when there are multiple matches"
  (test "should return a list with the multiple matches"
        '("strong" "live" "arp")
        (in-list '("live" "strong" "arp") haystack))))
