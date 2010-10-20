
(asdf:defsystem #:ldx.com
  :depends-on (#:trivial-features
               #:alexandria
               #:virgil
               #:ldx.windows
               #:closer-mop)
  :components ((:module "com"
                        :serial t
                        :components ((:file "package")
                                     (:file "interface")
                                     (:file "interface-defs")
                                     (:file "object")
                                     (:file "unknown")
                                     ))))

;; vim: ft=lisp et
