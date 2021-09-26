(import srfi-1)

;;;
;;; Find the integer that appears an odd number of times.
;;;
;;; List-Of(Integer) -> Integer | #f
;;;
(define (find-n xs)
  (find (lambda (x)
          (odd?
           (length
            (filter
             (lambda (i)(= x i))
             xs))))
        xs))
