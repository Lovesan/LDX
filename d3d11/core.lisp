(define-enum (driver-type
               (:base-type dword)
               (:conc-name driver-type-))
  :unknown
  :hardware
  :reference
  :null
  :software
  :warp)
