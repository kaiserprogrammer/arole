(asdf:defsystem arole-test
  :version "0"
  :description "test package for arole"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "BSD-style"
  :depends-on (lisp-unit arole)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "arole-test")))
