(import (chicken sort))
(import (only srfi-1 find filter))
(import (only srfi-13 string> string-contains))

;;
;; Checks whether `needle` is a substring of `haystack`.
;;
;; @curried
;;
;; @signature
;; String -> [String] -> Int | #f
;;
;; @example
;; ((contains "sh") "lisp")
;; ;; → #f
;;
;; ((contains "sh") "learn scheme!")
;; ;; → 5
;;
(define (contains needle)
  (lambda (haystack)
    (string-contains haystack needle)))

;;
;; Retruns a lexicographically sorted list of strings in `xs` which
;; are substrings of the strings in `ys`.
;;
;; ASSUME: parameters are valid lists of strings.
;;
;; @signature
;; [String] [String] -> [String]
;;
;; @example
;; (in-list '("hi") '("good-bye"))
;; ;; → ()
;;
;; (in-list '("live") '("Portal" "Still" "Alive"))
;; → ("live")
;;
(define (in-list xs ys)
  (sort
   (filter
    (lambda (x)
      (find (contains x) ys)) xs) string>))

