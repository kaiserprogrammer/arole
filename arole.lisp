(defpackage :arole
  (:use :cl :lisp-unit))

(in-package :arole)

(defvar *role-db*)

(defclass memory-db ()
  ((children :initform (make-hash-table)
             :accessor children)
   (users :initform (make-hash-table :test 'equal)
          :accessor users)
   (parents :initform (make-hash-table)
            :accessor parents)))

(defun add-role (role &key (db *role-db*) parents)
  (db-add-role role parents db))

(defun add-user (id role &key (db *role-db*))
  (db-add-user id role db))

(defun roles (id &key (db *role-db*))
  (db-get-user id db))

(defmethod db-get-user (id (db memory-db))
  (let ((base-role (gethash id (users db))))
    (list* base-role (gethash base-role (parents db)))))

(defmethod db-all-roles ((db memory-db))
  (alexandria:hash-table-keys (children db)))

(defmethod db-add-role (role parents (db memory-db))
  (setf (gethash role (children db)) nil)
  (dolist (p parents)
    (pushnew role (gethash p (children db))))
  (clrhash (parents db))
  (maphash (lambda (parent children)
             (dolist (child children)
               (push parent (gethash child (parents db)))))
           (children db)))

(defmethod db-add-user (id role (db memory-db))
  (setf (gethash id (users db)) role))

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
