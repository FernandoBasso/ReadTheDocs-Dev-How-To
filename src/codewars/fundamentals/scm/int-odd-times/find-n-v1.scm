(import (only srfi-1 find filter))

;;
;; `Result` is one of:
;; • `#f`
;; • `Int`
;;

;;
;; Finds the integer that appears an odd number of times.
;;
;; @sig ListOf<Int> -> Result
;;
(define (find-n xs)
  (find
   (lambda (x)
     (odd?
      (length
       (filter
        (lambda (i)(= x i))
        xs)))) xs))
