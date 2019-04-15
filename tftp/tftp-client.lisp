(require 'usocket)

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
