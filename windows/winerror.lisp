(in-package #:ldx.windows)

(define-results windows-status (warning)
  ()
  ((ok 0 "No error occurred")
   (false 1 "Successful but nonstandard completion of operation"))
  (:conc-name status-)
  (:default-initargs :code status-false))

(defun windows-status-code (windows-status)
  (declare (type windows-status windows-status))
  (slot-value windows-status 'code))

(define-results windows-error (error)
  ()
  ((success 0 "No error occurred")
   (unexpected-failure #x8000FFFF
     "Catastrophic failure")
   (not-implemented #x80004001
     "Not implemented")
   (out-of-memory #x8007000E
     "Ran out of memory")
   (invalid-arg #x80070057
     "One or more arguments are invalid")
   (no-interface #x80004002
     "No such interface supported")
   (invalid-pointer #x80004003
     "Invalid pointer")
   (invalid-handle #x80070006
     "Invalid handle")
   (abort #x80004004
     "Operation aborted")
   (failure #x80004005
     "Unspecified error")
   (access-denied #x80070005
     "General access denied error")
   (data-pending #x8000000A
     "The data necessary to complete this operation is not yet available"))
  (:conc-name error-)
  (:default-initargs :code error-failure))

(defun windows-error-code (windows-error)
  (declare (type windows-error windows-error))
  (slot-value windows-error 'code))

(define-external-function ("GetLastError" (:camel-case))
    (:stdcall kernel32)
  (dword))

(define-external-function ("SetLastError" (:camel-case))
    (:stdcall kernel32)
  (void)
  (error-code dword))

(defun last-error (&optional (error-if-no-error T) default-value)
  (let ((result (hresult-from-win32 (get-last-error))))
    (if (hresult-error-p result)
      (error 'windows-error :code result)
      (if error-if-no-error
        (error 'windows-error :code error-failure)
        default-value))))

(defun %last-error (value)
  (declare (ignore value))
  (last-error))

(defun error-assertion-failed (value)
  (error "Assertion on ~s failed" value))

(define-proxy-type asserted-type ()
  ((predicate-name :initform 'identity
                   :reader asserted-type-predicate-name
                   :initarg :predicate-name)
   (failure-handler-name :initform 'error-assertion-failed
                         :reader asserted-type-failure-handler-name
                         :initarg :failure-handler-name)))

(define-immediate-type immediate-asserted-type (asserted-type)
  ()
  (:translator (value type)
    (let ((value (translate-value value (proxied-type type))))
      (if (funcall (asserted-type-predicate-name type) value)
        value
        (funcall (asserted-type-failure-handler-name type) value))))
  (:translator-expansion (value-form type)
    (with-gensyms (value)
      `(let ((,value ,(expand-translate-value value-form (proxied-type type))))
         (declare (type ,(lisp-type type) ,value))
         (if (,(asserted-type-predicate-name type) ,value)
           ,value
           (,(asserted-type-failure-handler-name type) ,value))))))

(define-aggregate-type aggregate-asserted-type (asserted-type)
  ()
  (:reader (pointer out type)
    (let ((value (read-value pointer out (proxied-type type))))
      (if (funcall (asserted-type-predicate-name type) value)
        value
        (funcall (asserted-type-failure-handler-name type) value))))
  (:reader-expansion (pointer-form out-form type)
    (with-gensyms (value)
      `(let ((,value ,(expand-read-value pointer-form out-form (proxied-type type))))
         (declare (type ,(lisp-type type) ,value))
         (if (,(asserted-type-predicate-name type) ,value)
           ,value
           (,(asserted-type-failure-handler-name type) ,value))))))

(define-type-parser assert (type &optional (predicate 'identity)
                                 (failure-handler 'error-assertion-failed))
  (check-type predicate symbol)
  (check-type failure-handler symbol)
  (let ((type (parse-typespec type)))
    (if (or (primitive-type-p type)
            (immediate-type-p type))
      (make-instance 'immediate-asserted-type
        :type type :predicate-name predicate
        :failure-handler-name failure-handler)
      (make-instance 'aggregate-asserted-type
        :type type :failure-handler-name failure-handler
        :predicate-name predicate))))

(defmethod unparse-type ((type asserted-type))
  `(assert ,(unparse-type (proxied-type type))
     ,(asserted-type-predicate-name type)
     ,(asserted-type-failure-handler-name type)))

(declaim (inline not-null))
(defun not-null (x)
  (not (null x)))

(declaim (inline not-zero))
(defun not-zero (x)
  (not (zerop x)))

(defalias windows-assert (type &optional (predicate 'not-null))
  `(assert ,type ,predicate %last-error))
