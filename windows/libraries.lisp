(in-package :ldx.windows)

(define-foreign-library kernel32
  (T (:default "kernel32")))

(define-foreign-library user32
  (T (:default "user32")))

(define-foreign-library gdi32
  (T (:default "gdi32")))

(define-foreign-library ws2-32
  (T (:default "ws2_32")))

(use-foreign-library kernel32)
(use-foreign-library user32)
(use-foreign-library gdi32)
(use-foreign-library ws2-32)
