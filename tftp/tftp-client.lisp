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
                    (number-to-vector (car x) (cadr x))
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
          (a-list (cons tftp-value ',args))
          (aa-list (apply #'trans-args-to-mbuf-args  a-list)))
     (funcall #'merge-vectors-mbuf ,mbuf aa-list)))



(defun run ()
  (let ((client (usocket:socket-connect nil nil
                 :protocol :datagram
                 :timeout 10
                 )))
    (when client
      (build-tftp-msg :read +sbuf+ "test.txt" "netascii")
      (usocket:socket-send client
                           (message-data +sbuf+)
                           (message-len +sbuf+)
                           :port 69
                           :host "127.0.0.1")
      (print (message-data +sbuf+))
      (print (message-len +sbuf+))
      (multiple-value-bind (recv size remote-host remote-port)
          (usocket:socket-receive client (message-data +rbuf+) nil)
        (declare (ignore recv))
        (format t "recv data ~a from ~a:~a on socket ~a" (message-data +rbuf+) remote-host remote-port client)))))


