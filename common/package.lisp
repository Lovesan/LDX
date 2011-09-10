(in-package #:cl-user)

(defpackage #:ldx
  (:use #:cl #:alexandria #:virgil #:doors #:doors.com)
  (:export

    ;;common conditions
    #:ldx-condition
    #:ldx-error
    #:ldx-status
    #:ldx-error-invalid-call
    #:ldx-error-file-not-found
    #:ldx-error-too-many-unique-state-objects
    
    ;;common structures
    #:shader-macro    
    #:make-shader-macro
    #:shader-macro-p
    #:copy-shader-macro
    #:shader-macro-name
    #:shader-macro-definition    
    
    ;;common interfaces
    #:blob
    #:blob-pointer
    #:blob-size
    #:include
    #:include-open
    #:include-close
    ))
