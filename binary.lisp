;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; $Id$
;;;;

;;;;
;;;; Totally ripped from Peter Seibel.
;;;; http://www.gigamonkeys.com/book/
;;;;

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))


(defgeneric read-object (object stream &key)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream &key)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))


(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key &allow-other-keys)
  (assert (typep value type))
  (write-object value stream))


(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun direct-slots (name)
  (copy-list (getf (symbol-plist name) 'slots)))

(defun inherited-slots (name)
  (loop for super in (getf (symbol-plist name) 'superclasses)
     nconc (direct-slots super)
     nconc (inherited-slots super)))

;; this only works after d-b-c is expanded, since only then
;; has the plist been set. but we need that info sooner..
(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

;; so its done this way, a shortcut using the info 
;; we would use to set the plist
(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))



(defmacro define-generic-binary-class (name superclasses slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (getf (symbol-plist ',name) 'slots) ',(mapcar #'first slots))
	 (setf (getf (symbol-plist ',name) 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
	 ,(mapcar #'slot->defclass-slot slots))

       ,read-method
       
   (defmethod write-object progn ((,objectvar ,name) ,streamvar &key)
     (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
       ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))


(defmacro define-binary-class (name superclasses slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,objectvar ,name) ,streamvar &key)
	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
	 ,objectvar))))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

(defmacro define-tagged-binary-class (name superclasses slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
         (let ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
	   (let ((,objectvar
		   (make-instance
		    ,@(cdr (assoc :dispatch options))
		    ,@(mapcan #'slot->keyword-arg slots))))
	      (read-object ,objectvar ,streamvar)
	      ,objectvar))))))
 
(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) 
	   (mklist (first spec))
         `(progn
            (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
              (read-value ',derived-from ,stream ,@derived-args))
            (defmethod write-value 
		((,type (eql ',name)) ,stream ,value &key ,@args)
              (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
          ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
             `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
                ,@body))
          ,(destructuring-bind ((out value) &body body) 
	       (rest (assoc :writer spec))
             `(defmethod write-value 
		  ((,type (eql ',name)) ,out ,value &key ,@args)
                ,@body)))))))

