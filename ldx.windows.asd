
(asdf:defsystem #:ldx.windows
  :depends-on (#:trivial-features #:alexandria #:trivial-garbage #:virgil)
  :components ((:module "windows"
                        :serial t
                        :components ((:file "package")
                                     (:file "libraries")
                                     (:file "features")
                                     (:file "wintypes")
                                     (:file "osversion")
                                     (:file "hresult")
                                     (:file "winbase")
                                     (:file "guid")
                                     ))))

;; vim: ft=lisp et
