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

(defalias long-ptr () 'int-ptr)
(deftype long-ptr () 'int-ptr)
(defalias ulong-ptr () 'uint-ptr)
(deftype ulong-ptr () 'uint-ptr)

(defalias atom () 'word)

(deftype handle () '(or null pointer))

(define-immediate-type handle-type ()
  ()
  (:simple-parser handle)
  (:base-type pointer)
  (:lisp-type (type) 'handle)
  (:prototype (type) nil)
  (:prototype-expansion (type) nil)
  (:converter (value type)
    (or value &0))
  (:translator (value type)
    (and (&? value) value))
  (:converter-expansion (value type)
    `(or ,value &0))
  (:translator-expansion (value type)
    (with-gensyms (handle)
     `(let ((,handle ,value))
        (declare (type pointer ,handle))
        (and (&? ,handle) ,handle))))
  (:allocator-expansion (value type)
    `(alloc '*))
  (:deallocator-expansion (pointer type)
    `(free ,pointer '*))
  (:cleaner-expansion (pointer value type)
    nil))

(defalias astring (&optional length)
  `(string :encoding :ascii
           :byte-length ,(if length
                           (* length (sizeof 'char))
                           nil)))
(defalias wstring (&optional length)
  `(string :encoding :utf-16le
           :byte-length ,(if length
                           (* length (sizeof 'wchar))
                           nil)))

(defalias tstring (&optional length)
  #+ldx.unicode `(wstring ,length)
  #-ldx.unicode `(astring ,length))

(defalias tchar ()
  #+ldx.unicode 'wchar
  #-ldx.unicode 'char)
