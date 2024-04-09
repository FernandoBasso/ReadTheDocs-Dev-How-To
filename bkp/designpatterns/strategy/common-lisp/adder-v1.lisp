;;
;; One way to run:
;;
;;     sbcl --quit --load adder-v1.lisp
;;

(defun is-even (num)
  (evenp num))

(defun is-odd (num)
  (oddp num))

(defparameter *nums* (list 1 2 3 4))

;;
;; NOTE: `filter` is a function.
;;
(defun addNums (nums filter)
  (let ((total 0))
    (dolist (n nums total)
      ;;
      ;; Call `filter` passing it the `n` as argument.
      ;;
      (when (funcall filter n)
        (incf total n)))))

;;
;; Pass a reference of `is-even` and `is-odd` functions.
;;

(print (addNums *nums* #'is-even))
(print (addNums *nums* #'is-odd))

;;
;; Sum all. Just returning `arg` means returning
;; a truthy value in this case. Pass a function created
;; right then and there.
;;
(print (addNums *nums* (lambda (arg) arg)))

;;
t;; Sum none. We invert the truthy value of `arg`
;; to return nil. Also pass a function created right
;; then and there.
;;
(print (addNums *nums* (lambda (arg) (not arg))))

;;
;; Note that `lambda`s are also just passed, but
;; called inside `addNums` just like `is-even`
;; and `is-odd`.
;;
