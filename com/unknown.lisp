(in-package #:ldx.com)

(define-interface unknown
    ((iid-unknown #x00000000 #x0000 #x0000
                  #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))
  "Lisp wrapper for IUnknown inteface"
  (query-interface (hresult rv (translate pointer (iid-is iid)))
    (iid (& iid))
    (pointer (& pointer :out) :aux))
  (add-ref (ulong))
  (release (ulong)))

(defun finalize-unknown (object)
  (declare (type unknown object))
  (let ((pobject (com-interface-pointer object)))
    (declare (type pointer pobject))
    (finalize object
              (lambda ()
                (external-pointer-call
                  (deref (&+ (deref pobject '*) 2 'pointer)
                         'pointer)
                  ((:stdcall)
                   (ulong)
                   (pointer))
                  pobject)))))

(defmacro with-interface ((var interface) &body body)
  `(let ((,var ,interface))
     (unwind-protect
         (locally ,@body)
       (release ,var))))

(defmacro with-interfaces ((&rest specs) &body body)
  (if (null specs)
    `(locally ,@body)
    `(with-interface ,(car specs)
       (with-interfaces ,(rest specs)
         ,@body))))

#|

(define-iid iid-adder #xCAFEBABE 0 0 1 2 3 4 5 6 7 8)

(define-interface adder (iid-adder unknown)
  (add (int) (x int) (y int)))

(defclass adder-object (com-object)
  ())

(defmethod add ((object adder-object) x y)
  (values (+ x y) x y))

(defvar *adder-object* (make-instance 'adder-object))

(defvar *adder-object-unknown* (acquire-interface* *adder-object* 'unknown))

(defvar *adder-object-adder* (query-interface *adder-object-unknown* iid-adder))

(format t "~s : ~s"
        '(add *adder-object-adder* 1 2)
        (add *adder-object-adder* 1 2))

(finalize *adder-object-unknown*
          (lambda ()
            (format t "~&~s disposing~%" '*adder-object-unknown*)))
(finalize *adder-object-adder*
          (lambda ()
            (format t "~&~s disposing~%" '*adder-object-adder*)))
(finalize *adder-object*
          (lambda ()
            (format t "~&~s disposing~%" '*adder-object*)))

(makunbound '*adder-object*)
(makunbound '*adder-object-unknown*)
(makunbound '*adder-object-adder*)

(define-interface stack ()
  (stack-push (void) (value int))
  (stack-pop (boolean rv (if rv out))
             (out (& int :out) :aux)))

(defclass stack-object (com-object)
  ((stack :initform '()
          :initarg :stack
          :accessor stack-object-stack)))

(defmethod stack-push ((object stack-object) value)
  (push value (stack-object-stack object))
  (values nil value))

(defmethod stack-pop ((object stack-object))
  (if (null (stack-object-stack object))
    (values nil 0)
    (values t (pop (stack-object-stack object)))))

|#
