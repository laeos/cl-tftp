;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; $Id$
;;;;
;;;; tftp.lisp - a simple (!) TFTP implementation.
;;;;
;;;; Currently relies on SBCL for networking stuff.
;;;; It is pretty broken though, I can't figure out how
;;;; to do a sendmsg without connecting the socket.
;;;;
;;;;
;;;; TODO:
;;;;   fix braindead timeout handling (commented out cuz its broken)
;;;;   do tftp server
;;;;   callback for status infos
;;;;   asdf package
;;;;   asdf-install'able?
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
    (require 'sb-bsd-sockets)
    (use-package :sb-bsd-sockets))

(defconstant +tftp-server-port+ 69
  "UDP port that the TFTP server listens on.")

(defconstant +tftp-block-size+ 512
  "How much file data is sent in a DATA packet.")

(defconstant +tftp-max-packet-size+
  (+ +tftp-block-size+ 4)
  "What is the max packet length?")

(defparameter +tftp-opcode-list+
  '((rrq-frame . 1)
    (wrq-frame . 2)
    (data-frame . 3)
    (ack-frame . 4)
    (error-frame . 5))
  "List of opcodes and their associated frame format.")

(defclass tftp-session ()
  ((type :type (member rrq-frame wrq-frame)
	 :accessor sess-type
	 :initarg :type
	 :initform 'rrq-frame)
   (blocknum :type '(unsigned-byte 16)
	     :accessor sess-blocknum
	     :initform 0)
   (remote-file :type 'simple-string
		:reader sess-remote-file
		:initarg :remote-file)
   (local-file :type 'simple-string
	       :reader sess-local-file
	       :initarg :local-file)
   (remote-host :reader sess-remote-host
		:initarg :remote-host)
   (remote-port :accessor sess-remote-port
		:initarg :remote-port
		:initform +tftp-server-port+)
   (remote-server-port :accessor sess-remote-server-port
		       :initarg :remote-server-port
		       :initform +tftp-server-port+)
   (file :accessor sess-file
	 :initarg :file)
   (mode :type 'simple-string
	 :reader sess-mode
	 :initarg :mode)
   (finished :type 'integer
	     :accessor sess-finished
	     :initform 0)
   (timeout :type 'integer
	    :reader sess-timeout
	    :initform 3
	    :initarg :timeout)
   (retries :type 'integer
	    :reader sess-retries
	    :initform 1
	    :initarg :retries)
   (socket :reader sess-socket
	   :initform (error "need socket")
	   :initarg :socket)
   (progress :reader progress
	     :initarg :progress
	     :initform nil)
   (frame-len :accessor sess-frame-len
	    :initform 0)
   (packet-buffer :accessor sess-packet-buffer
		  :initform (make-array +tftp-max-packet-size+
					:fill-pointer +tftp-max-packet-size+
					:element-type '(unsigned-byte 8)))
   (stream :reader sess-stream
	   :initarg :stream)))


;;;
;;; some fun socket operations
;;;

(defun get-host-address (host)
  "Call the resolver, get an address vector."
  (sb-bsd-sockets:host-ent-address       
   (sb-bsd-sockets:get-host-by-name host)))

(defun make-udp-socket (&key (local-host #(0 0 0 0)) (local-port 0))
  "Connect to remote tftp server, return new socket."
  (let ((socket (make-instance
		 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) 1)
    (sb-bsd-sockets:socket-bind socket local-host local-port)
    socket))

(defmacro with-udp-socket ((socket &rest rest) &body body)
  `(let ((,socket (make-udp-socket ,@rest)))
     (unwind-protect
	  (progn
	    ,@body)
       (sb-bsd-sockets:socket-close ,socket))))

(defun receive (socket buffer)
  (socket-receive socket buffer nil))

;;;
;;; Types for the binary format reader.
;;; Everything works on a vector of (unsigned-byte 8).
;;;

(define-binary-type unsigned-integer (bytes)
  (:reader (in)
	   (loop for shift downfrom (* 8 (1- bytes)) to 0 by 8
		 for part = (ash (vector-pop in) shift)
		 for value = part then (logior value part)
		 finally (return value)))
  (:writer (out value)
	   (loop for shift from (* -8 (1- bytes)) to 0 by 8
		 do (write-byte (logand (ash value shift) #xff) out))))


(define-binary-type u1 () (unsigned-integer :bytes 1))
(define-binary-type u2 () (unsigned-integer :bytes 2))

(define-binary-type null-terminated-string ()
  (:reader (in)
	   (with-output-to-string (s)
	     (loop for char = (code-char (vector-pop in))
		   until (char= char #\Null)
		   do (write-char char s))))
  (:writer (out string)
	   (declare (type simple-array string))
	   (loop for char across string
		 do (write-byte (char-code char) out)
		 finally (write-byte 0 out))))

(define-binary-type data-block ()
  (:reader (in)
	   (reverse in))
  (:writer (out data)
	   (write-sequence data out)))

(defun find-opcode-class (id)
  "Given a TFTP opcode, return the class that implements it."
  (let ((it (car (rassoc id +tftp-opcode-list+))))
    (unless it
      (error "unknown TFTP opcode ~D" id))
    it))

(define-tagged-binary-class tftp-frame ()
  ((opcode u2))
  (:dispatch (find-opcode-class opcode)))

(define-binary-class rrq-frame (tftp-frame)
  ((file null-terminated-string)
   (mode null-terminated-string)))

(define-binary-class wrq-frame (tftp-frame)
  ((file null-terminated-string)
   (mode null-terminated-string)))

(define-binary-class data-frame (tftp-frame)
  ((blocknum u2)
   (data     data-block)))

(define-binary-class ack-frame (tftp-frame)
  ((blocknum u2)))

(define-binary-class error-frame (tftp-frame)
  ((errorcode  u2)
   (errormsg   null-terminated-string)))

(defmethod write-object :around ((packet tftp-frame) s &key)
  "Set opcode for this packet."
  (declare (optimize (speed 3)))
  (setf (opcode packet)
	(cdr (assoc packet +tftp-opcode-list+ :test #'typep)))
  (call-next-method))

(defun new-tftp-buffer (&optional (size +tftp-block-size+))
  "Return a new 512 byte array."
  (make-array size
	      :fill-pointer size
	      :element-type '(unsigned-byte 8)))

(defun read-block (session blocknum)
  "Read a block from a file."
  (let ((stream (sess-file session))
	(buffer (new-tftp-buffer)))
    (when (file-position stream (* +tftp-block-size+ blocknum))
      (setf (fill-pointer buffer)
	    (read-sequence buffer stream))
      buffer)))

(defun write-block (session blocknum buffer)
  "Write a block to a file"
  (let ((stream (sess-file session)))
    (if (file-position stream (* +tftp-block-size+ blocknum))
	(write-sequence buffer stream)
	(error "TFTP could not write block number ~D" blocknum))))


(defun send-to (socket stream host port packet)
  (socket-connect socket host port)
  (write-object packet stream)
  (force-output stream)
  (socket-connect socket host 0))

(defun send-packet (session packet)
  "Send a packet!"
  (send-to (sess-socket session)
	   (sess-stream session)
	   (sess-remote-host session)
	   (sess-remote-port session)
	   packet))

(defun recv-packet (session)
  "Return a TFTP! Also sets remote port on first packet."
  (loop
     (with-slots (socket remote-server-port remote-port)
	 session
       (let ((buffer (sess-packet-buffer session)))
	 (setf (fill-pointer buffer) +tftp-max-packet-size+)
	 (multiple-value-bind (buf len addr port)
	     (receive socket buffer)
	   (declare (ignore addr buf))
	   (setf (fill-pointer buffer) len)
	   (when (= remote-port remote-server-port)
	     (setf remote-port port))
	   (when (= remote-port port)
	     (return (read-value 'tftp-frame (nreverse buffer)))))))))

(defun request-reply (session packet)
  "Send the current packet, return reply or timeout"
  (flet ((try-once ()
	   (send-packet session packet)
	   (recv-packet session)))
    (try-once)))
    

;    (dotimes (count (1- (sess-retries session)))
;      (handler-bind ((timeout #'(lambda (x) (declare (ignore x)) (go again))))
;	(return (try-once)))
;      again)
;    (try-once))) ; one more try. let it throw timeout.

(defun sending-file-p (session)
  "Return true if this is a put session."
  (eq (sess-type session) 'wrq-frame))

(defun getting-file-p (session)
  "Return true if this is a get session."
  (eq (sess-type session) 'rrq-frame))

(defgeneric handle-packet (session packet)
  (:documentation "Handle a packet. Returns reply packet."))

(defmethod handle-packet ((session tftp-session) (packet error-frame))
  "Signal an error!"
  (with-accessors ((code errorcode) (msg errormsg)) packet
    (error "TFTP ERROR: ~D: ~S" code msg)))

(defmethod handle-packet ((session tftp-session) (packet ack-frame))
  "Got an ack. Send next data frame."
  (when (sending-file-p session)
    (if (and (< (sess-frame-len session) +tftp-block-size+)
	     (not (= (blocknum packet) 0)))
	(setf (sess-finished session) 1)
	(let* ((blocknum (blocknum packet))
	       (data (read-block session blocknum)))
	  (setf (sess-frame-len session) (length data))
	  (make-instance 'data-frame 
			 :blocknum (1+ blocknum)
			 :data data)))))

;; finished after < block-size block
(defmethod handle-packet ((session tftp-session) (packet data-frame))
  "Got a data frame, send an ack."
  (when (getting-file-p session)
    (let ((blocknum (blocknum packet))
	  (data (data packet)))
      (when (= blocknum (sess-blocknum session))	
	(when (< (length data) +tftp-block-size+)
	  (setf (sess-finished session) 1))
	(write-block session (1- blocknum) data)
	(incf (sess-blocknum session)))
      (make-instance 'ack-frame :blocknum blocknum))))

(defun start-packet (session)
  "Generate the first packet of the session."
  (make-instance (sess-type session)
		 :file (sess-remote-file session)
		 :mode (sess-mode session)))

(defun finish-getting-file (session packet)
  "Wait to see if the other guy got our last ack."
)
;  (handler-case
;      (request-reply session packet)
;    (timeout () nil)))

(defun client-loop (session)
  "request, reply. request, reply. Will the work never end?"
  (let ((packet (start-packet session)))
    (loop
       (setq packet (handle-packet session 
				   (request-reply session packet)))
       (when (= (sess-finished session) 1)
	 (if (getting-file-p session)
	     (finish-getting-file session packet))
	 (return)))))

(defun tftp-get (session)
  "tftp-get specific session setup"
  (with-open-file (stream (sess-local-file session)
		   :direction :output 
		   :if-exists :supersede
		   :if-does-not-exist :create
		   :element-type '(unsigned-byte 8))
    (setf (sess-file session) stream
	  (sess-type session) 'rrq-frame
	  (sess-blocknum session) 1)
    (client-loop session)))

(defun tftp-put (session)
  "tftp-put specific session setup"
  (with-open-file (stream (sess-local-file session) :direction :input)
    (setf (sess-file session) stream
	  (sess-type session) 'wrq-frame)
    (client-loop session)))

(defun make-stream (socket)
  "Helper, just call socket-make-stream."
  (socket-make-stream socket 
		      :input t 
		      :output t 
		      :buffering :full 
		      :element-type '(unsigned-byte 8)))

(defun tftp (type &key local-file
                       remote-file
	               remote-host
      	               (remote-port +tftp-server-port+)
	               (local-host #(0 0 0 0))
                       (local-port 0)
                       (timeout 1) 
                       (retries 3)
	               (mode "octet")
	               progress)
  "Do a TFTP!"
  (with-udp-socket (socket :local-host local-host :local-port local-port)
    (funcall 
     (ecase type
       (:get   #'tftp-get)
       (:put   #'tftp-put))
     (make-instance 'tftp-session
		    :local-file (or local-file remote-file)
		    :remote-file (or remote-file local-file)
		    :remote-port remote-port
		    :remote-server-port remote-port
		    :remote-host (get-host-address remote-host)
		    :timeout timeout
		    :retries retries
		    :mode mode
		    :socket socket
		    :progress progress
		    :stream (make-stream socket)))))
