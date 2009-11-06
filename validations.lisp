(defpackage :validations
  (:use :common-lisp
	:closer-mop)
  (:shadowing-import-from :closer-mop :defmethod :standard-generic-function :defmethod :defgeneric :standard-generic-function)
  (:export :validation
	   :validatable-class
	   :validatable-object
	   :name
	   :offending-keys
	   :check
	   :message
	   :validations
	   :validate
	   :valid-p
	   :offending-validation-keys))

(in-package :validations)

(defclass validation ()
  ((name :initarg :name
	 :initform nil
	 :reader name)
   (offending-keys :initarg :offending-keys
		   :initform nil
		   :reader offending-keys)
   (check :initarg :check
	  :initform nil
	  :reader check)
   (message :initarg :message
	    :initform ""
	    :accessor message)))

(defclass validatable-class (standard-class)
  ((validations :accessor validations
		:initform nil
		:initarg :validations))
  (:documentation "A class that holds validations"))

(defmethod ensure-class-using-class :after ((vc validatable-class) name &rest args &key validations)
  (declare (ignore name args))
  (setf (validations vc) (map 'list 'eval validations)))

(defmethod validate-superclass ((vc validatable-class) (sc standard-class)) T)

(defclass validatable-object () ()
  (:metaclass validatable-class))

(defgeneric validate (object)
  (:documentation "Validates the given object with respect to the validations that have been added to it"))
(defgeneric valid-p (object)
  (:documentation "Returns T if the given object is valid, returns false in the other case"))
(defgeneric offending-validation-keys (object &optional comprator)
  (:documentation "Returns the keys that are offending the validations.  No order is guaranteed, doubles are removed."))

(defmethod validate ((object validatable-object))
  (loop for validation in (validations (class-of object)) 
     when (not (funcall (check validation) object))
     collect validation))
(defmethod valid-p (object)
  (not (validate object)))
(defmethod offending-validation-keys ((object validatable-object) &optional (comparator 'eql))
  (remove-duplicates 
   (loop for validation in (validate object) 
      append (offending-keys validation))
   :test comparator))

;;;;;;;;;;;;;;
;; example use
;;;;;;;;;;;;;;

;; (defclass user (validatable-object)
;;   ((name :accessor name
;; 	 :initarg :name)
;;    (email :accessor email
;; 	  :initarg :email)
;;    (nick :accessor nick
;; 	 :initarg :nick))
;;   (:metaclass validatable-class)
;;   (:validations (make-instance 'validation 
;; 			       :name 'email-is-valid
;; 			       :offending-keys '(email foobar)
;; 			       :message "Email must be in the form of \"name@domain.extention\"!"
;; 			       :check (lambda (object) (cl-ppcre:scan "\\w+@\\w+\\.\\w+" (email object))))
;; 		(make-instance 'validation
;; 			       :name 'nick
;; 			       :offending-keys '(nick)
;; 			       :message "Nick must be at least three characters long"
;; 			       :check (lambda (object) (> (length (nick object)) 3)))
;; 		(make-instance 'validation
;; 			       :name 'name
;; 			       :offending-keys '(name nick email)
;; 			       :message "You must enter your full name"
;; 			       :check (lambda (object) (cl-ppcre:scan ".+ .+" (name object))))))

;; (defvar *test-user* (make-instance 'user
;; 				   :name "Real Name"
;; 				   :email "valid@email.com"
;; 				   :nick "test-user"))

;; (valid-p *test-user*)
;; (validate *test-user*)
;; (setf (email *test-user*) "not@valid")
;; (valid-p *test-user*)
;; (validate *test-user*)
;; (setf (name *test-user*) "invalid")
;; (valid-p *test-user*)
;; (validate *test-user*)
;; (map 'list 'name (validate *test-user*))
;; (map 'list 'message (validate *test-user*))
;; (map 'list 'offending-keys (validate *test-user*))
;; (offending-validation-keys *test-user*)