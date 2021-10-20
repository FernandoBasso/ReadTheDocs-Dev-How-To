(defpackage :leslies-lists
  (:use :cl)
  (:export :new-list
           :list-of-things
           :add-to-list
           :first-thing
           :second-thing
           :third-thing
           :twenty-third-thing
           :remove-first-item
           :on-the-list-p
           :list-append
           :just-how-long
           :part-of-list
           :list-reverse))

(in-package :leslies-lists)

;;
;; Creates a new, empty list.
;;
;; @signature
;; new-list :: _ -> ()
;;
;; @example
;; (new-list)
;; ;; → nil
;;
(defun new-list () '())

;;
;; Creates a list of three things.
;;
;; @signature
;; list-of-things :: a a a -> (a a a)
;;
;; @example
;; (list-of-things 'a 'b 'c)
;; ;; → (A B C)
;;
(defun list-of-things (arg1 arg2 arg3)
  (list arg1 arg2 arg3))

;;
;; Adds an element in front of a list.
;;
;; @signature
;; add-to-list :: a [a] -> [a]
;;
;; @example
;; (add-to-list 'hello '())
;; ;; (HELLO)
;;
;; (add-to-list 3 '(1 2))
;; ;; → (3 1 2)
;;
(defun add-to-list (item list)
  (cons item list))

;;
;; Returns the first element of a list or `nil` if list is empty.
;;
;; @signature
;; first-thing :: [a] -> a
;;
;; @example
;; (first-thing '())
;; ;; → nil
;;
;; (first-thing '(may the force))
;; ;; → MAY
;;
(defun first-thing (list)
  (car list))

;;
;; Returns the second element of a list or `nil` if there is no second element.
;;
;; @signature
;; second-thing :: [a] -> a
;;
;; @example
;; ;; There is no second element in this list.
;; (second-thing '())
;;
;; ;; There is not second element in this list.
;; (second-thing '(x))
;; ;; → NIL
;;
;; (second-thing '(x y z))
;; ;; → Z
;;
(defun second-thing (list)
  (car (cdr list)))

;;
;; Returns the third element of a list or `nil` if there is no third element.
;;
;; @signature
;; third-thing :: [a] -> a
;;
;; @example
;; ;; There is not third element on this list.
;; (third-thing '())
;; ;; → NIL
;;
;; ;; There is second element, but no third element in this list.
;; (third-thing '(w x))
;; ;; → NIL
;;
;; (third-thing '(w x y))
;; ;; → Y
;;
;; (third-thing '(w x y z))
;; ;; → Y
;;
(defun third-thing (list)
  (nth 2 list))

;;
;; Returns the 23rd element of a list, or `nil` if there is no 23rd element.
;;
;; @signature
;; twenty-third-thing :: [a] -> a
;;
(defun twenty-third-thing (list)
  (nth 22 list))

;;
;; Removes the first element of a list.
;;
;; @signature
;; remove-first-item :: [a] -> [a]
;;
;; @example
;; (remove-first-item '())
;; ;; → NIL
;;
;; (remove-first-item '(x y z))
;; ;; → (Y Z)
;;
(defun remove-first-item (list)
  (cdr list))

;;
;; Appends the second list to the first list.
;;
;; @signature
;; list-append :: [a] [a] -> [a]
;;
;; @example
;; (list-append '() '(x y))
;; ;; → (X Y)
;;
;; (list-append '(x y) '())
;; ;; → (X Y)
;;
;; (list-append '(1 2) '(y z))
;; ;; → (1 2 Y Z)
;;
(defun list-append (list1 list2)
  (append list1 list2))

;;
;; Returns the number of elements in a list.
;;
;; @signature
;; just-how-long :: [a] -> Int
;;
;; @example
;; (just-how-long '())
;; ;; → 0
;;
;; (just-how-long '("hello" "world"))
;; ;; → 2
;;
;; (just-how-long '(42 :foo '(a b c)))
;; ;; → 3
;;
(defun just-how-long (list)
  (length list))
