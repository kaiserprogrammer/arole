(defpackage :arole-test
  (:use :cl :lisp-unit :arole))

(in-package :arole-test)

(remove-tests :all)

(defmacro with-setup (&body body)
  `(let ((*role-db* (make-instance 'memory-db)))
     (add-role :programmer)
     ,@body))

(define-test adding-a-role
  (with-setup
    (assert-equal (list :programmer) (arole::db-all-roles *role-db*))))

(define-test adding-a-user
  (with-setup
    (add-user "jim" :programmer)
    (assert-equal (list :programmer) (roles "jim"))))

(define-test adding-multiple-hierarchy
  (with-setup
    (add-role :student)
    (add-role :coder :parents (list :student))
    (add-user "john" :coder)
    (assert-equal (list :coder :student) (roles "john"))
    (add-role :cowboy-coder :parents (list :manager :programmer))
    (add-user "three" :cowboy-coder)
    (assert-equal (list :cowboy-coder :manager :programmer) (roles "three"))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
