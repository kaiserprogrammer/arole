# A|Role

Is a system which unifies roles for users. A role can have multiple
parentroles.

```lisp
(let ((*role-db* (make-instance 'memory-db)))
         (add-role :programmer)
         (add-user "john" :programmer)
         (member :programmer (roles "john")))
```
