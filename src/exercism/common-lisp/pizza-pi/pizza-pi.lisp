(defpackage :pizza-pi
  (:use :cl)
  (:export
   :dough-calculator
   :pizzas-per-cube
   :size-from-sauce
   :fair-share-p))

(in-package :pizza-pi)

;;
;; Calculates the amount of dough required to make `num-pizzas` pizzas
;; of diameter `diameter`.
;;
;; @signature
;; dough-calculator :: Num a => a a -> a
;;
;; n * (((45 * pi * d) / 20) + 200)
;;
;; (* (+ (/ (* 45 pi 30) 20) 200) 4)
;;
;; @example
;; (dough-calculator 4 30)
;; ;; → 1684
;;
;; (dough-calculator 6 20)
;; ;; → 2048
;;
(defun dough-calculator (num-pizzas diameter)
  (round (* (+ (/ (* 45 pi diameter) 20) 200) num-pizzas)))

;;
;; Calculates pizza diameter from the quantity of sauce.
;;
;; @signature
;; size-from-sauce :: Num a => a -> a
;;
;; @example
;; (size-from-sauce 250)
;; ;; → 32.573500793528d0
;;
(defun size-from-sauce (sauce)
  (sqrt (/ (* 40 sauce) (* 3 pi))))

;;
;; Calculates how many pizzas of `diameter` diameter can be made with
;; `cube-size` cubes of cheese.
;;
;; @signature
;; pizzas-per-cube :: Num a => a a -> a
;;
(defun pizzas-per-cube (cube-size diameter)
  (floor (/ (* 2 (expt cube-size 3)) (* 3 pi (expt diameter 2)))))

;;
;; Checks whether we can fairly divide `pizzas` pizzas among `friends`
;; friends.
;;
;; Each pizza as 8 slices. If we can evenly divide the total slices of
;; pizzas among the friends, we have fair share.
;;
;; @signature
;; fair-share-p :: Int Int -> Bool
;;
(defun fair-share-p (pizzas friends)
  (= 0 (mod (* 8 pizzas) friends)))
