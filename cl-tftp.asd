;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; $Id$
;;;;

(in-package #:cl-user)

(defpackage #:cl-tftp.system
  (:use #:cl
	#:asdf))

(in-package #:cl-tftp.system)

(defsystem #:cl-tftp
    :name "cl-tftp"
    :author "Rex Feany"
    :version "0.0.0"
    :description "Common Lisp interface to the TFTP protocol."
    :depends-on
       #+sbcl (:sb-bsd-sockets)
    :components ((:file "package")
		 (:file "binary" :depends-on ("package"))
		 (:file "cl-tftp" :depends-on ("package" "binary"))))


