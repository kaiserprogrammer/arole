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
