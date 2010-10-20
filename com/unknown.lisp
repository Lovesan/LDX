(in-package #:ldx.com)

(define-interface unknown
    ((iid-unknown #x00000000 #x0000 #x0000
                  #xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))
  "Lisp wrapper for IUnknown inteface"
  ;;Query interface is just a stub for now.
  ;;See real method definition further.
  ;;This is required because we do not support recursive types.
  ;;And this is also the reason of presence of DEFINE-INTERFACE-METHOD macro.
  (query-interface (hresult)
    (iid pointer)
    (pout pointer :aux))
  (add-ref (ulong))
  (release (ulong)))

;;This is the true method
(define-interface-method unknown query-interface
  (hresult rv
    (translate-interface
      (com-interface-pointer interface)
      (or (find-interface-class-by-iid iid nil)
          (progn (when interface (release interface))
                 (error 'windows-error :code error-no-interface)))))
  (iid (& iid))
  (interface (& (unknown nil) :out) :aux))

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
