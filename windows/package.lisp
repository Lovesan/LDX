(in-package :cl-user)

(defpackage #:ldx.windows
  (:use #:cl #:alexandria #:virgil)
  (:export
    
    ;;windows types, constructors and accessors
    #:word
    #:dword
    #:qword
    #:wparam
    #:lparam
    #:hresult
    #:lresult
    #:handle
    #:rect
    #:rect-left
    #:rect-top
    #:rect-right
    #:rect-bottom
    #:point
    #:pt-x
    #:pt-y
    #:point-s
    #:pt-s-x
    #:pt-s-y
    #:size
    #:size-cx
    #:size-cy
    #:filetime
    #:filetime-low-date-time
    #:filetime-high-date-time
    
    ;;utility functions
    #:make-short
    #:make-word
    #:make-long
    #:make-dword
    #:make-long-long
    #:make-qword
    #:low-dword
    #:high-dword
    #:low-word
    #:high-word
    #:low-byte
    #:high-byte
    
    ;;uuid and friends
    #:uuid-of
    #:uuid-is
    #:register-uuid
    
    #:uuid
    #:uuid-dw
    #:uuid-w1
    #:uuid-w2
    #:uuid-b1
    #:uuid-b2
    #:uuid-b3
    #:uuid-b4
    #:uuid-b5
    #:uuid-b6
    #:uuid-b7
    #:uuid-b8
    #:uuid-p
    #:copy-uuid
    #:define-uuid
    #:with-uuid-accessors
    #:uuid-equal
    #:uuid-of
    #:uuid-is
    
    #:guid
    #:guid-dw
    #:guid-w1
    #:guid-w2
    #:guid-b1
    #:guid-b2
    #:guid-b3
    #:guid-b4
    #:guid-b5
    #:guid-b6
    #:guid-b7
    #:guid-b8
    #:guid-p
    #:copy-guid
    #:define-guid
    #:define-ole-guid
    #:with-guid-accessors
    #:guid-equal
    
    #:iid
    #:iid-dw
    #:iid-w1
    #:iid-w2
    #:iid-b1
    #:iid-b2
    #:iid-b3
    #:iid-b4
    #:iid-b5
    #:iid-b6
    #:iid-b7
    #:iid-b8
    #:iid-p
    #:copy-iid
    #:define-iid
    #:with-iid-accessors
    #:iid-equal
    
    #:clsid
    #:clsid-dw
    #:clsid-w1
    #:clsid-w2
    #:clsid-b1
    #:clsid-b2
    #:clsid-b3
    #:clsid-b4
    #:clsid-b5
    #:clsid-b6
    #:clsid-b7
    #:clsid-b8
    #:clsid-p
    #:copy-clsid
    #:define-clsid
    #:with-clsid-accessors
    #:clsid-equal
    
    #:fmtid
    #:fmtid-dw
    #:fmtid-w1
    #:fmtid-w2
    #:fmtid-b1
    #:fmtid-b2
    #:fmtid-b3
    #:fmtid-b4
    #:fmtid-b5
    #:fmtid-b6
    #:fmtid-b7
    #:fmtid-b8
    #:fmtid-p
    #:copy-fmtid
    #:define-fmtid
    #:with-fmtid-accessors
    #:fmtid-equal
    ))