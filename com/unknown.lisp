(in-package #:ldx.com)

(define-interface unknown
    ((iid-unknown #x00000000 #x0000 #x0000
                  #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))
  "Lisp wrapper for IUnknown inteface"
  (query-interface (hresult rv
                     (let ((class (find-interface-class-by-iid iid nil)))
                       (unless class
                         (when (&? pointer)
                           (external-pointer-call
                             (deref (&+ (deref pointer '*) 2 '*) '*)
                             ((:stdcall)
                              (ulong)
                              (pointer this :aux pointer))))
                         (error 'windows-error :code error-no-interface))
                       (translate-interface pointer (class-name class))))
    (iid (& iid))
    (pointer (& pointer :out) :aux))
  (add-ref (ulong))
  (release (ulong)))

(defmethod shared-initialize :after
  ((object unknown) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (when (com-interface-finalizable-p object)
    (let ((pobject (com-interface-pointer object)))
      (declare (type pointer pobject))
      (when (&? pobject)
        (finalize object (lambda ()
                           (external-pointer-call
                             (deref (&+ (deref pobject '*) 2 '*) '*)
                             ((:stdcall)
                              (ulong)
                              (pointer this :aux pobject))))))))
  object)

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
