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

(defun push-item-to-mbuf (e mbuf)
  (if (stringp e)
      (loop for ch across e do
            (add-end (char-code ch) mbuf))
      (if (numberp e)
          (add-end e mbuf)
          (error "Can not add type ~a to mbuf" (type-of e)))))



;; the args must be strings or integer
(defun build-packet (dst-mbuf &rest args)
  (dolist (x args) (push-item-to-mbuf x dst-mbuf)))


(defun build-message (type &rest args)
  (case type
    (:read (build-packet +sbuf+ 1 (first args) 0 (second args) 0))
    (:write (build-packet +sbuf+ 2 (first args) 0 (second args) 0))
    (:ack (build-packet +sbuf+ 4 (first args)))
    (:data (build-packet +sbuf+ 3 (first args) (second args)))
    (:error (build-packet +sbuf+ 5 (first args)))
    (t (error "tftp not support this type ~a" type))))

(defun run ()
  (let ((client (usocket:socket-connect
                 "127.0.0.1"
                 69
                 :protocol :datagram )))
    (when client
      (build-message :read "test.txt" "octet")
      (usocket:socket-send client
                           (message-data +sbuf+)
                           (message-len +sbuf+)
                           :host "127.0.0.1"
                           :port 69)
      (usocket:socket-receive client
                       (message-data +rbuf+)
                       1024)
      (print (message-data +rbuf+)))))


(defun push-number-to-mbuf (element len mbuf)
  (when (> len 0)
    (let ((start (if (> (- len 9) 0) (- len 9) 0)))
      (push-item-to-mbuf (ldb (byte 8 start) element) mbuf)
      (push-number-to-mbuf element (- len 8) mbuf))))

(defun apend-mbuf (m1 m2)
  (dotimes (i (message-len m2))
    (push-number-to-mbuf (aref (message-data m2) i) 8 m1)))

(defun string-to-vector (strs)
  (let ((v (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for c across strs do
          (vector-push-extend (char-code c) v))
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

(defun merge-vectors-mbuf (mbuf &rest vectors)
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

(defmacro build-tftp-msg (tftp-type mbuf &rest args)
  `(let ((arg ,(case tftp-type
                 (:read (progn
                          (push-number-to-mbuf (coerce 1 '(unsigned-byte 16)) 16 `,mbuf)
                          (push-item-to-mbuf (first args) `,mbuf)
                          (push-number-to-mbuf (coerce 0 '(unsigned-byte 8)) 8 `,mbuf)
                          (push-item-to-mbuf (second args) `,mbuf)
                          (push-number-to-mbuf (coerce 0 '(unsigned-byte 8)) 8 `,mbuf)))
                 (:write (progn
                           (push-number-to-mbuf (coerce 2 '(unsigned-byte 16)) 16 `,mbuf)
                           (push-item-to-mbuf (first args) `,mbuf)
                           (push-number-to-mbuf (coerce 0 '(unsigned-byte 8)) 8 `,mbuf)
                           (push-item-to-mbuf (second args) `,mbuf)
                           (push-number-to-mbuf (coerce 0 '(unsigned-byte 8)) 8 mbuf)))
                  (:data (progn
                          (push-number-to-mbuf (coerce 3 '(unsigned-byte 16)) 16 mbuf)
                          (push-number-to-mbuf (coerce (first args) '(unsigned-byte 16)) 16 mbuf)
                          (append-mbuf mbuf (second args))))
                  (:ack (progn
                         (push-number-to-mbuf (coerce 4 '(unsigned-byte 16)) 16 mbuf)
                         (push-number-to-mbuf (coerce (first args) '(unsigned-byte 16)) 16 mbuf)))
                  (:error (progn
                           (push-number-to-mbuf (coerce 5 '(unsigned-byte 16)) 16 mbuf)
                           (push-number-to-mbuf (coerce (first args) '(unsigned-byte 16)) 16 mbuf)
                           (push-item-to-mbuf (second args) mbuf)
                           (push-number-to-mbuf (coerce 0 '(unsigned-byte 8)) 8 mbuf))))))
     (print "push ok")))




