;;
;; Calculates the factorial using tail recursion.
;;
;; This version requires an initial second parameter of 1.
;;
;; Usage:
;;
;;   (fact 120 1)
;;   (fact (* 512 * 32) 1)
;;
(define (fact-bkp n acc)
  (cond
   ((<= n 1) acc)
   (else
    (fact-bkp (- n 1) (* acc n)))))

;;
;; Calculates the factorial using tail recursion.
;;
;; This solution does NOT require an additional second parameter of 1.
;; It internally provides that. It is relieves the user for the burden
;; of having to worry about that parameter and it is therefore generally
;; considered a better approach.
;;
;; Usage:
;;
;;   (fact 120)
;;   (fact (* 512 32))
;;
(define (fact n)
  (let go ((x n) (acc 1))
    (cond
     ((< x 1) acc)
     (else
      (go (- x 1) (* acc x))))))

