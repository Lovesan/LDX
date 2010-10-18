
(asdf:defsystem #:ldx.com
  :depends-on (#:trivial-features #:alexandria #:virgil #:ldx.windows)
  :components ((:module "com"
                        :serial t
                        :components ((:file "package")
                                     (:file "interface")
                                     (:file "object")
                                     (:file "unknown")
                                     ))))

;; vim: ft=lisp et
