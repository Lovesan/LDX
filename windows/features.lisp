(in-package #:ldx.windows)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (ignore-errors
          (with-pointer (p "test" '(string :encoding :utf-16le))
            (deref p '(string :encoding :utf-16le))))
    (pushnew :ldx.unicode *features*)))
