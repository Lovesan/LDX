
(asdf:defsystem #:ldx.windows
  :depends-on (#:trivial-features #:alexandria #:trivial-garbage #:virgil)
  :components ((:module "windows"
                        :serial t
                        :components ((:file "package")
                                     (:file "libraries")
                                     (:file "features")
                                     (:file "wintypes")
                                     (:file "hresult")
                                     (:file "winerror")
                                     (:file "osversion")
                                     (:file "winnt")
                                     (:file "uuid")
                                     (:file "winbase")
                                     ))))

;; vim: ft=lisp et
