;;
;; `Result` is one of:
;; • `#f`
;; • `Int`
;;

(define (count-occurrences x xs)
  (let loop ((lst xs))
    (if (null? lst)
        0
        (+ (if (= (car lst) x) 1 0)
           (loop (cdr lst))))))

;;
;; Finds the int that appears an odd number of times.
;;
;; @sig ListOf<Int> -> Result
;;
(define (find-n lst)
  (let loop ((nums lst))
    (if (null? lst)
        #f
        (let ((cur-num (car nums)))
          (if (odd? (count-occurrences cur-num lst))
              cur-num
              (loop (cdr nums)))))))
