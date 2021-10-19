(defpackage :socks-and-sexprs
  (:use :cl)
  (:export :lennys-favorite-food :lennys-secret-keyword
           :is-an-atom-p :is-a-cons-p :first-thing :rest-of-it))

(in-package :socks-and-sexprs)

;;
;; Evaluates to some symbol (not a keyword).
;;
;; NOTE: Always evaluates to the symbol `'vegetables`.
;;
;; _ -> Symbol
;;
(defun lennys-favorite-food ()
  'vetegables)

;;
;; Evaluates to some keyword.
;;
;; NOTE: Always evaluate to the keyword `:other-lifeforms-exist`
;;
;; _ -> Keyword
;;
(defun lennys-secret-keyword ()
  :other-lifeforms-exist)

;;
;; Evaluates to `T` if THING is an atom, `NIL` otherwise
;;
;; @signature
;; * -> Bool
;;
;; @example
;; (is-an-atom-p 1)
;; ;; → T
;;
;; (is-an-atom-p '())
;; ;; → t
;;
;; (is-an-atom-p '(1 2))
;; ;; → NIL
;;
(defun is-an-atom-p (thing)
  (atom thing))

;;
;; Evaluates to `T` if THING is a cons, `NIL` otherwise
;;
;; @signature
;; * -> Bool
;;
;; @example
;; (is-a-cons-p 'x)
;; ;; → NIL
;;
;; (is-a-cons-p '())
;; ;; → NIL
;;
;; (is-a-cons-p '(1 2))
;; ;; → T
;;
(defun is-a-cons-p (thing)
  (consp thing))

;;
;; Evaluates to the first part of `cons`.
;;
;; @signature
;; * -> Bool
;;
;; @example
;; (first-thing '(:x :y))
;; ;; → :X
;;
(defun first-thing (cons)
  (car cons))

;;
;; Evaluates to the 'rest' of `cons`.
;;
;; @example
;; (rest-of-it '(1 2 3))
;; ;; → '(2 3)
;;
(defun rest-of-it (cons)
  (cdr cons))
