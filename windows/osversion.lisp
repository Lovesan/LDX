(in-package #:ldx.windows)

(define-enum (version-suite (:conc-name ver-suite-)
                            (:base-type word))
  (:backoffice #x00000004)
  (:blade #x00000400)
  (:compute-server #x00004000)
  (:datacenter #x00000080)
  (:enterprise #x00000002)
  (:embedded-nt #x00000040)
  (:personal #x00000200)
  (:single-user-ts #x00000100)
  (:small-business #x00000001)
  (:small-business-restricted #x00000020)
  (:storage-server #x00002000)
  (:terminal #x00000010)
  (:home-server #x00008000))

(define-enum (version-product-type
               (:conc-name ver-nt-)
               (:base-type byte))
  (:domain-controller #x00000002)
  (:server #x00000003)
  (:workstation #x00000001))

(define-struct (os-version-info-ex
                 (:conc-name osverinfo-))
  (size dword)
  (major-version dword)
  (minor-version dword)
  (build-number dword)
  (platform-id dword)
  (csd-version (wstring 128))
  (service-pack-major word)
  (service-pack-minor word)
  (suite-mask word)
  (product-type version-product-type)
  (reserved byte))

(define-external-function
    ("GetVersion" (:camel-case))
    (:stdcall kernel32)
  (dword))

(load-time-value
  (defconstant winnt-version  (get-version)))

(define-external-function
    (#+ldx.unicode "GetVersionExW"
     #-ldx.unicode "GetVersionExA"
                 get-version-ex)
    (:stdcall kernel32)
  (boolean rv (when rv
                version-info))
  (version-info (& os-version-info-ex :inout)
                :aux
                (make-os-version-info-ex
                  :size (sizeof 'os-version-info-ex))))

(let ((info (get-version-ex)))
  (when info
    (pushnew (case (osverinfo-major-version info)
               (5 (case (osverinfo-minor-version info)
                    (0 :win2000)
                    (1 :winxp)
                    (2 (cond
                         ((eq (osverinfo-product-type info) :workstation)
                          :winxp64)
                         ((logand (osverinfo-suite-mask info)
                                  ver-suite-home-server)
                          :winhomeserver)
                         (T :winserver2003)))))
               (6 (case (osverinfo-minor-version info)
                    (0 (if (eq (osverinfo-product-type info)
                               :workstation)
                         :winvista
                         :winserver2008))
                    (1 (if (eq (osverinfo-product-type info)
                               :workstation)
                         :win7
                         :winserver2008r2))
                    (T :windows)))
               (T :windows))
             *features*)))
