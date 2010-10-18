(in-package #:ldx.windows)

(defalias word () 'uint16)
(deftype word () 'uint16)

(defalias dword () 'uint32)
(deftype dword () 'uint32)

(defalias qword () 'uint64)
(deftype qword () 'uint64)

(defalias wparam () 'uint-ptr)
(deftype wparam () 'uint-ptr)
(defalias lparam () 'int-ptr)
(deftype lparam () 'int-ptr)
(defalias lresult () 'int-ptr)
(deftype lresult () 'int-ptr)

(defalias handle () 'pointer)
(deftype handle () 'pointer)

(declaim (inline make-short))
(defun make-short (a b)
  (lognot (logand #xFFFF
                  (lognot
                    (logior (logand a #xFF)
                            (ash (logand b #xFF) 8))))))

(declaim (inline make-word))
(defun make-word (a b)
  (logior (logand a #xFF)
          (ash (logand b #xFF) 8)))

(declaim (inline make-long))
(defun make-long (a b)
  (lognot (logand #xFFFFFFFF
                  (lognot
                    (logior (logand a #xFFFF)
                            (ash (logand b #xFFFF) 16))))))

(declaim (inline make-dword))
(defun make-dword (a b)
  (logand #xFFFFFFFF
          (logior (logand a #xFFFF)
                  (ash (logand b #xFFFF) 16))))

(declaim (inline make-long-long))
(defun make-long-long (a b)
  (lognot (logand #xFFFFFFFFFFFFFFFF
                  (lognot
                    (logior (logand a #xFFFFFFFF)
                            (ash (logand b #xFFFFFFFF) 32))))))

(declaim (inline make-qword))
(defun make-qword (a b)
  (logand #xFFFFFFFFFFFFFFFF
          (logior (logand a #xFFFFFFFF)
                  (ash (logand b #xFFFFFFFF) 32))))

(declaim (inline low-dword))
(defun low-dword (x)
  (logand x #xFFFFFFFF))

(declaim (inline high-dword))
(defun high-dword (x)
  (logand (ash x -32) #xFFFFFFFF))

(declaim (inline low-word))
(defun low-word (x)
  (logand x #xFFFF))

(declaim (inline high-word))
(defun high-word (x)
  (logand (ash x -16) #xFFFF))

(declaim (inline low-byte))
(defun low-byte (x)
  (logand x #xFF))

(declaim (inline high-byte))
(defun high-byte (x)
  (logand (ash x -8) #xFF))

(define-struct (rect
                 (:constructor rect (left top right bottom)))
  (left long)
  (top long)
  (right long)
  (bottom long))

(define-struct (point
                 (:constructor point (x y))
                 (:conc-name pt-))
  (x long)
  (y long))

(define-struct (size
                 (:constructor size (cx cy)))
  (cx long)
  (cy long))

(define-struct (point-s
                 (:constructor point-s (x y))
                 (:conc-name pt-s-))
  (x short)
  (y short))

(define-struct (filetime
                 (:constructor filetime (low-date-time
                                         high-date-time)))
  (low-date-time dword)
  (high-date-time dword))
