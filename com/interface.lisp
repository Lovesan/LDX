(in-package #:ldx.com)

(defvar *pointer-to-object-mapping* (make-weak-hash-table :test #'eql
                                      :weakness :value))

(defvar *pointer-to-interface-mapping* (make-weak-hash-table :test #'equal
                                         :weakness :value))

(defvar *iid-to-interface-class-mapping* (make-hash-table :test #'equalp))

(eval-when (:compile-toplevel :load-toplevel :execute)

(closer-mop:defclass com-interface-class (standard-class)
  ((iid :initform nil :initarg :iid)   
   (vtable-name :initform nil
                :initarg :vtable-name
                :reader com-interface-class-vtable-name)
   (methods :initform '()
            :initarg :methods
            :reader com-interface-class-methods)))

(defmethod closer-mop:validate-superclass
    ((class standard-class) (superclass com-interface-class))
  nil)

(defmethod closer-mop:validate-superclass
    ((class com-interface-class) (superclass standard-class))
  t)
  
(closer-mop:defclass com-interface ()
  ((com-pointer :initform &0 :initarg :pointer)
   (finalizable :initform t :initarg :finalizable
                :reader com-interface-finalizable-p))
  (:metaclass com-interface-class))
  
(closer-mop:finalize-inheritance (find-class 'com-interface-class))
(closer-mop:finalize-inheritance (find-class 'com-interface))

) ;;eval-when

(defun find-interface-class (name &optional (errorp T))
  (declare (type symbol name))
  (let ((class (find-class name errorp)))
    (if (typep class 'com-interface-class)
      class
      (and errorp (error 'windows-error :code error-no-interface)))))

(defun find-interface-class-by-iid (iid &optional (errorp T))
  (declare (type iid iid))
  (let ((class (gethash iid *iid-to-interface-class-mapping*)))
    (if (typep class 'com-interface-class)
      class
      (and errorp (error 'windows-error :code error-no-interface)))))

(defconstant pointer-slot-location
    (closer-mop:slot-definition-location
        (find 'com-pointer
              (closer-mop:class-slots (find-class 'com-interface))
              :key #'closer-mop:slot-definition-name)))

(defconstant iid-slot-location
    (closer-mop:slot-definition-location
        (find 'iid
              (closer-mop:class-slots (find-class 'com-interface-class))
              :key #'closer-mop:slot-definition-name)))

(declaim (inline com-interface-pointer))
(defun com-interface-pointer (interface)
  (declare (type com-interface interface))
  (closer-mop:standard-instance-access interface pointer-slot-location))

(declaim (inline com-interface-method-pointer))
(defun com-interface-method-pointer (interface method-name)
  (declare (type com-interface interface)
           (type symbol method-name))
  (let* ((class (class-of interface))
         (index (or (position method-name
                              (com-interface-class-methods class)
                              :test #'eq)
                    (error "Interface ~s has no method named ~s"
                           (class-name class) method-name))))
    (deref (deref (com-interface-pointer interface) 'pointer)
           'pointer
           (* index (sizeof 'pointer)))))

(defmethod uuid-of ((class com-interface-class))
  (closer-mop:standard-instance-access class iid-slot-location))

(defun translate-interface (pointer name &optional (finalizable T))
  (declare (type pointer pointer)
           (type symbol name)
           (optimize (speed 3)))
  (if (&? pointer)
    (let* ((address (the size-t (&& pointer)))
           (typed-pointer (cons address name))
           (interface (gethash typed-pointer *pointer-to-interface-mapping*)))
      (unless interface
          (setf (gethash typed-pointer *pointer-to-interface-mapping*)
                (setf interface (make-instance name :pointer pointer
                                  :finalizable finalizable))))
      interface)
    nil))

(declaim (inline convert-interface))
(defun convert-interface (interface)
  (declare (type com-interface interface))
  (com-interface-pointer interface))

(define-immediate-type com-interface-type ()
  ((name :initform nil
         :initarg :name
         :reader com-interface-type-name)
   (finalizable :initform T
                :initarg :finalizable
                :reader com-interface-type-finalizable-p))
  (:base-type pointer)
  (:lisp-type (type) (com-interface-type-name type))
  (:converter (lisp-value type)    
    (convert-interface lisp-value))
  (:translator (pointer type)
    (translate-interface pointer
                         (com-interface-type-name type)
                         (com-interface-type-finalizable-p type)))
  (:converter-expansion (lisp-value-form type)
    `(convert-interface ,lisp-value-form))
  (:translator-expansion (pointer-form type)
    `(translate-interface ,pointer-form
                          ',(com-interface-type-name type)
                          ,(and (com-interface-type-finalizable-p type)
                                T)))
  (:allocator-expansion (value type)
    `(alloc 'pointer))
  (:deallocator-expansion (pointer type)
    `(free ,pointer '*))
  (:cleaner-expansion (pointer value type)
    nil))

(defmethod unparse-type ((type com-interface-type))
  (com-interface-type-name type))
