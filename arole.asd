(asdf:defsystem arole
  :version "0"
  :description "Unify user roles"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "BSD-style"
  :depends-on (alexandria)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "arole")))
