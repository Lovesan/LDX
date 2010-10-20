(in-package #:ldx.com)

(defvar *registered-com-objects* (make-array 0 :adjustable t :fill-pointer 0))

(defclass com-object ()
  ((ref-count :initform 0
              :accessor com-object-ref-count)
   (interface-pointers :initform (make-hash-table :test #'eq)
                       :reader com-object-interface-pointers)))

(defmethod shared-initialize :after ((object com-object) slot-names
                                      &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (unless (find object *registered-com-objects* :test #'eq)
    (finalize object (let ((pointers (com-object-interface-pointers object)))
                       (lambda ()
                         (maphash (lambda (k v)
                                    (declare (ignore k)) (free v 'pointer))
                           pointers)))))
  object)

(defgeneric query-interface (object name))
(defgeneric add-ref (object))
(defgeneric release (object))

(defun acquire-interface (object name)
  (declare (type com-object object) (type symbol name))
  (let ((interface-pointer (gethash name (com-object-interface-pointers object))))
    (if interface-pointer
      (progn
        (add-ref object)
        (or (gethash (&& interface-pointer) *pointer-to-interface-mapping*)
            (setf (gethash (&& interface-pointer) *pointer-to-object-mapping*)
                  object
                  (gethash (&& interface-pointer) *pointer-to-interface-mapping*)
                  (funcall (com-interface-type-constructor-name
                             (gethash name *com-interface-types*))
                           interface-pointer))))
      (let* ((type (or (gethash name *com-interface-types*)
                       (error 'windows-error :code error-no-interface)))
             (vtable (symbol-value (com-interface-type-vtable-name type)))
             (interface-pointers (com-object-interface-pointers object))
             (pointer (or (gethash name interface-pointers)
                          (setf (gethash name interface-pointers)
                                (alloc 'pointer vtable))))
             (interface (funcall (com-interface-type-constructor-name type)
                                 pointer)))
        (add-ref object)
        (setf (gethash (&& pointer) *pointer-to-object-mapping*) object
              (gethash (&& pointer) *pointer-to-interface-mapping*) interface)))))

(defun acquire-interface* (object name)
  (declare (type com-object object) (type symbol name))
  (finalize (acquire-interface object name)
            (lambda () (release object))))

(defmethod query-interface ((object com-object) (name uuid))
  (values nil
          name
          (com-interface-pointer (acquire-interface object (iid-is name)))))

(defmethod add-ref ((object com-object))
  (or (position object *registered-com-objects* :test #'eq)
      (vector-push-extend object *registered-com-objects*))
  (incf (com-object-ref-count object)))

(defmethod release ((object com-object))
  (let ((ref-count (com-object-ref-count object)))
    (if (> ref-count 0)
      (progn
        (when (= ref-count 1)
          (let ((pos (position object *registered-com-objects* :test #'eq)))
            (when (> pos (1+ (length *registered-com-objects*)))
              (replace *registered-com-objects*
                       *registered-com-objects*
                       :start1 pos :start2 (1+ pos)))
            (adjust-array *registered-com-objects*
                          (1- (length *registered-com-objects*))
                          :fill-pointer t)))
        (decf (com-object-ref-count object)))
      0)))
