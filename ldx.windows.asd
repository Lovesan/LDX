
(asdf:defsystem #:ldx.windows
  :depends-on (#:trivial-features #:alexandria #:trivial-garbage #:virgil)
  :components ((:module "windows"
                        :serial t
                        :components ((:file "package")
                                     (:file "winbase")
                                     (:file "guid")
                                     ))))

;; vim: ft=lisp et
