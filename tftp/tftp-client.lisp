(require 'usocket)

(defpackage :tftp-moment
  (:use :common-lisp)
  )

(in-package :tftp-moment)
;; the message struct
(defstruct message
  (len 0)
  data)


(defconstant +sbuf+
  (make-message
   :len 0
   :data (make-array 512 :element-type '(unsigned-byte 8))))

(defconstant +rbuf+
  (make-message
   :len 0
   :data (make-array 512 :element-type '(unsigned-byte 8))))

(defun add-end (e mbuf)
  (progn
    (setf (aref (message-data mbuf) (message-len mbuf)) e)
    (incf (message-len mbuf))))

(defun string-to-vector (strs)
  (let ((v (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for c across strs do
          (vector-push-extend (char-code c) v))
    (vector-push-extend 0 v)
    v))

(defun number-to-vector (n size)
  (labels ((n-t-v (n size v)
             (if (<= size 0)
                 v
                 (progn
                   (vector-push-extend (ldb (byte 8
                                                  (- size 8))
                                            n)
                                       v)
                   (n-t-v n (- size 8) v)))))
    (let ((v (make-array 0
                         :element-type '(unsigned-byte 8)
                         :adjustable t
                         :fill-pointer 0)))
      (n-t-v n size v)
      v)))


;; rewrite trans-args-to-mbuf-args
;; args is a list, but each of it must eval
;; first, so the element is not a list

(defun trans-args-to-mbuf-args (&rest args)
  (let ((one (first args))
        (second (second args)))
    (when one
      (print one)
      (if (stringp one)
          (cons (string-to-vector one) (apply #'trans-args-to-mbuf-args (cdr args)))
          (if (numberp one)
              (cons (number-to-vector one second) (apply #'trans-args-to-mbuf-args (cddr args)))
              (error "Moment: trans-args-to-mbuf-args unkown object ~a" one))))))

(defun merge-vector-mbuf (mbuf vector)
  (let ((len (length vector)))
    (dotimes (i len)
      (add-end (aref vector i) mbuf))))

(defun merge-vectors-mbuf (mbuf vectors)
  (dolist (v vectors)
    (merge-vector-mbuf mbuf v)))



(defmacro build-tftp-msg (tftp-type mbuf &key file-name
                                              (file-mode "netascci")
                                              (seq-number 0)
                                              data
                                              errormsg)
  `(let ((v2
           (trans-args-to-mbuf-args
            ,@(case tftp-type
                (:read `(1 16 ,file-name ,file-mode))
                (:write `(2 16 ,file-name ,file-mode))
                (:data `(3 16  ,seq-number 16 ,data))
                (:ack `(4 16 ,seq-number 16))
                (:error `(5 16 ,seq-number 16 ,errormsg))))))
     (merge-vectors-mbuf ,mbuf v2)))



(defun tftp-data-p (vector-buf)
  (and (= 0 (aref vector-buf 0))
       (= 3 (aref vector-buf 1))))

(defun tftp-data-number (vector-buf)
  (+ (* (aref vector-buf 2) 256)
     (aref vector-buf 3)))

(defmacro new-ack (ack-num)
  `(let ((v (make-message :len 0
                         :data (make-array 4 :element-type '(unsigned-byte 8)))))
    (build-tftp-msg :ack v :seq-number ,ack-num)
     (message-data v)))


(defun write-n-bytes (stream vector-buf &key (start 0) end)
  (let ((endbytes (or end (length vector-buf))))
    (loop for i from start below endbytes do
          (write-byte (aref vector-buf i) stream))))


(defun read-file-from-server (socket srcfilename &optional dstfilename)
  (let ((target-name (or dstfilename srcfilename)))
    (with-open-file (stream target-name
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (loop
        (multiple-value-bind (recv size remote-host remote-port)
            (usocket:socket-receive socket (message-data +rbuf+) nil)
          (declare (ignore recv))
          (setf (message-len +rbuf+) 0)
          (when (tftp-data-p (message-data +rbuf+))
            (write-n-bytes stream (message-data +rbuf+) :start 4 :end size)
            (format t "recv : buf ~a, size ~a" (message-data +rbuf+) size)
            (usocket:socket-send socket (new-ack (+ 1 (tftp-data-number (message-data +rbuf+)))) 4 :host remote-host :port remote-port)
            (format t "send ack number ~a" (tftp-data-number (message-data +rbuf+)))
              (if (< size 512)
                  ;; we have done read just break
                  (return)))
          (when (not (tftp-data-p (message-data +rbuf+)))
              (format t "is not data message buf is ~a" (message-data +rbuf+)))
          )))))

(defun create-client-socket ()
  ;; random port
  (usocket:socket-connect nil nil
                          :protocol :datagram
                          :timeout 10))

(defun read-file-from-server% (operation filename)
  (let ((client (create-client-socket)))
    (declare (ignore operation))
    (setf (message-len +sbuf+) 0)
    (build-tftp-msg :read +sbuf+ :file-name filename  :file-mode "octet")
    (usocket:socket-send client (message-data +sbuf+) (message-len +sbuf+) :port 69 :host "127.0.0.1")
    (format t "has send ")
    (read-file-from-server client filename)
    (usocket:socket-close client)))
