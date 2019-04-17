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

(defun trans-args-to-mbuf-args (&rest args)
  (mapcar (lambda (x)
            (if (stringp x)
                (string-to-vector x)
                (if (consp x)
                    (number-to-vector (eval (car x)) (cadr x))
                    (error "trans-args-to-mbuf-args unknown x ~a" x))))
          args))

(defun merge-vector-mbuf (mbuf vector)
  (let ((len (length vector)))
    (dotimes (i len)
      (add-end (aref vector i) mbuf))))

(defun merge-vectors-mbuf (mbuf vectors)
  (dolist (v vectors)
    (merge-vector-mbuf mbuf v)))

(defmacro build-tftp-msg (tftp-type mbuf &rest args)
  `(let* ((tftp-value
           ',(case `,tftp-type
              (:read '(1 16))
              (:write '(2 16))
              (:data '(3 16))
              (:ack '(4 16))
              (:error '(5 16))
              (t "error tftp-type ~a" `,tftp-type)))
          (aa-list (apply #'trans-args-to-mbuf-args  tftp-value ,args)))
     (funcall #'merge-vectors-mbuf ,mbuf aa-list)))

(defmacro build-tftp-msg% (tftp-type mbuf &key file-name
                                               (file-mode "netascci")
                                               (seq-number 0)
                                               data
                                               errormsg)
  `(let* ((v1
           ',(case tftp-type
                     (:read (list '(1 16) `,`,file-name file-mode))
                     (:write (list '(2 16) `,`,file-name file-mode))
                     (:data (list '(3 16) (list seq-number 16) data))
                     (:ack (list '(4 16) (list seq-number 16)))
                     (:error (list '(5 16) (list seq-number 16) errormsg))))
          (v2  (apply #'trans-args-to-mbuf-args v1)))
     (funcall #'merge-vectors-mbuf ,mbuf v2)))


(defun tftp-data-p (vector-buf)
  (and (= 0 (aref vector-buf 0))
       (= 3 (aref vector-buf 1))))

(defun tftp-data-number (vector-buf)
  (+ (* (aref vector-buf 2) 256)
     (aref vector-buf 3)))

(defun new-ack (ack-num)
  (let ((v (make-message :len 0
                         :data (make-array 4 :element-type '(unsigned-byte 8)))))
    (build-tftp-msg% :ack v :seq-number ack-num)
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
          (when (tftp-data-p (message-data +rbuf+))
            (write-n-bytes stream (message-data +rbuf+) :start 4 :end (- size 4))
            (format t "recv : buf ~a, size ~a" (message-data +rbuf+) size)
            (usocket:socket-send socket (new-ack (+ 1 (tftp-data-number (message-data +rbuf+)))) 4 :host remote-host :port remote-port)
              (if (< size 512)
                ;; not the end , continue set ack back

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
    (setf (message-len +rbuf+) 0)
    (build-tftp-msg% :read +rbuf+ :file-name filename :file-mode "octet")
    (usocket:socket-send client (message-data +sbuf+) (message-len +sbuf+) :port 69 :host "127.0.0.1")
    (read-file-from-server client filename)
    (usocket:socket-close client)))
