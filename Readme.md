# A|Role

Is a system which unifies roles for users. A role can have multiple
parentroles.

```lisp
(let ((*role-db* (make-instance 'memory-db)))
  (add-role :programmer)
  (add-user "john" :programmer)
  (member :programmer (roles "john")))
```

a more elaborate example

```lisp
(let ((*role-db* (make-instance 'memory-db)))
  (add-role :student)
  (add-role :coder :parents (list :student))
  (add-user "john" :coder)
  (assert-equal (list :coder :student) (roles "john"))
  (add-role :cowboy-coder :parents (list :manager :coder))
  (add-user "three" :cowboy-coder)
  (assert-equal (list :cowboy-coder :manager :coder :student) (roles "three"))))
```