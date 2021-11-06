(import (only srfi-1 find filter))

;;
;; `Result` is one of:
;; • `#f`
;; • `Int`
;;

;;
;; Checks whether `x` equals `y`
;;
;; @curry
;;
;; @sig Int -> Int -> Bool
;;
;; @example
;; ((eqx? 1) 1)
;; ;; → #t
;;
;; @example
;; ((eqx? 1) -1)
;; ;; → #f
;;
;; @example
;; (define one? (eqx? 1))
;; (one? 1)
;; ;; → #t
;; (one? -1)
;; ;; → #f
;;
;(define ((eqx? x) y) (= x y))
(define (eqx? x)
  (lambda (y)
    (= x y)))

;;
;; Checks whether there is an odd number of `x` in `xs`.
;;
;; Int ListOf<Int> -> ListOf<Int>
;;
(define (oddx? x xs)
  (odd? (length (filter (eqx? x) xs))))

;;
;; Finds the int that appears an odd number of times.
;;
;; ListOf<Int> -> Result
;;
(define (find-n xs)
  (find (lambda (curX)
          (oddx? curX xs)) xs))
