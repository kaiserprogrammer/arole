(defpackage :arole
  (:use :cl :lisp-unit))

(in-package :arole)

(defvar *role-db*)

(defclass memory-db ()
  ((children :initform (make-hash-table)
             :accessor children)
   (users :initform (make-hash-table :test 'equal)
          :accessor users)))

(defun add-role (role &key (db *role-db*))
  (db-add-role role db))

(defun add-user (id role &key (db *role-db*))
  (db-add-user id role db))

(defun roles (id &key (db *role-db*))
  (db-get-user id db))

(defmethod db-get-user (id (db memory-db))
  (gethash id (users db)))

(defmethod db-all-roles ((db memory-db))
  (alexandria:hash-table-keys (children db)))

(defmethod db-add-role (role (db memory-db))
  (setf (gethash role (children db)) nil))

(defmethod db-add-user (id role (db memory-db))
  (pushnew role (gethash id (users db) (list))))

(remove-tests :all)

(defmacro with-setup (&body body)
  `(let ((*role-db* (make-instance 'memory-db)))
     (add-role :programmer)
     ,@body))

(define-test adding-a-role
  (with-setup
    (assert-equal (list :programmer) (db-all-roles *role-db*))))

(define-test adding-a-user
  (with-setup
    (add-user "jim" :programmer)
    (assert-equal (list :programmer) (roles "jim"))))


(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
