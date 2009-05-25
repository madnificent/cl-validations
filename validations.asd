(defpackage :validations.sysdef
  (:use :common-lisp :asdf))
(in-package :validations.sysdef)

(defsystem :validations
  :name "validations"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Trivial system to allow for the validating of objects."
  :depends-on (:closer-mop)
  :components ((:file "validations")))