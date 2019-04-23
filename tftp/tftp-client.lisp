(require 'usocket)

(defpackage :tftp-moment
  (:use :common-lisp)
  )

(in-package :tftp-moment)


(defparameter *block-num* 0
  "The block number of the data or ack")

(defun read-block-number (data-buf)
  (let ((n 0))
    (setf (ldb (byte 8 8) n) (aref data-buf 2))
    (setf (ldb (byte 8 0) n) (aref data-buf 3))
    n))

(defun read-operater-number (data-buf)
  (let ((n 0))
    (setf (ldb (byte 8 8) n) (aref data-buf 0))
    (setf (ldb (byte 8 0) n) (aref data-buf 1))
    n))

(defun process-read-error (data-buf)
  (let ((n (read-block-number data-buf))
        (str (binary-data-to-null-string data-buf :start 4)))
    (format t "ERROR CODE : ~a, ERROR MSG : ~a~%" n str))
	
;; Process Read file from Server
(defun write-value (obj dest)
  (if (numberp obj)
      (vector-push-extend obj dest)
      (if (stringp obj)
          (progn
            (loop for c across obj do
                  (vector-push-extend (char-code c) dest))
            (vector-push-extend 0 dest))
          (error "Wrong obj ~a for write-value" obj))))

(defun tftp-get (client-socket server-addr server-port filename &optional (mode "octet"))
  (declare (ignore mode))
  ;; 1. send RRQ
  (let ((v (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (write-value 0 v)
    (write-value 1 v)
    (write-value filename v)
    (write-value "octet" v)

    (format t "v is ~a, len is ~a ~%" v (length v))
    
    (usocket:socket-send client-socket v (length v) :host server-addr :port server-port)
    (with-open-file (stream filename :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop
        (multiple-value-bind (recv size remote-host remote-port)
            (usocket:socket-receive client-socket nil 520)
          (let ((result (handle-get-message recv size stream)))
            (when result
              (format t "result is : ~a~%" result))
            (if result
                (usocket:socket-send client-socket result (length result) :host remote-host :port remote-port)
                (return))

            (when (< size 512) (return))))))

    (usocket:socket-close client-socket)))


(defun handle-get-message (recv size stream)
  (let ((mtype (read-operater-number recv))
        (block-num (read-block-number recv)))
    (if (= 5 mtype)
        ;; process error msg
        (progn (process-read-error recv) nil)
        (if (= 3 mtype)
            (progn
              (when (= block-num *block-num*)
                (write-sequence recv stream :start 4 :end size)
                (incf *block-num*))
              (let ((ack (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
                (write-value 0 ack)
                (write-value 4 ack)
                (write-value (ldb (byte 8 8) *block-num*) ack)
                (write-value (ldb (byte 8 0) *block-num*) ack)
                ack))
            (format t "get can not process message type ~a" mtype)))))

(defun split-string-with-del (string del)
  (loop for i = 0 then (+ j 1)
        as j = (position del string :start i)
        collect (subseq string i j)
        while j))


(defun run ()
  (let ((host "127.0.0.1")
        (port 69)
        (client-socket (usocket:socket-connect nil nil :protocol :datagram)))
    (when (not client-socket)
      (format t "socket error")
      (return-from run))
    
    (format t "> ")
    (let* ((user-input (read-line))
           (user-input2 (string-trim " " user-input))
           (user-list (split-string-with-del user-input2 #\space)))
      (when (= 3 (length user-list))
        (setf host (third user-list))
        (tftp-get client-socket host port (second user-list)))
      (format t "process get file ~a done. ~%" (second user-list)))))
