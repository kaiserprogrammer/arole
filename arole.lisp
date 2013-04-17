(defpackage :arole
  (:use :cl)
  (:export
   :memory-db
   :add-role
   :add-user
   :roles
   :*role-db*))

(in-package :arole)

(defvar *role-db*)

(defclass memory-db ()
  ((users :initform (make-hash-table :test 'equal)
          :accessor users)
   (parents :initform (make-hash-table)
            :accessor parents)))

(defun add-role (role &key (db *role-db*) parents)
  (db-add-role role parents db))

(defun add-user (id role &key (db *role-db*))
  (db-add-user id role db))

(defun roles (id &key (db *role-db*))
  (db-get-roles id db))

(defmethod db-get-roles (id (db memory-db))
  (let ((base-role (gethash id (users db))))
    (get-all-parents base-role db)))

(defmethod get-all-parents (role (db memory-db))
  (let ((parents (list role)))
    (dolist (p (gethash role (parents db)))
      (setf parents (append parents (get-all-parents p db))))
    parents))

(defmethod db-all-roles ((db memory-db))
  (alexandria:hash-table-keys (parents db)))

(defmethod db-add-role (role parents (db memory-db))
  (setf (gethash role (parents db)) parents))

(defmethod db-add-user (id role (db memory-db))
  (setf (gethash id (users db)) role))
