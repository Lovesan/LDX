;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010-2011, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:ldx)

(define-results ldx-condition ()
  ()
  ()
  (:conc-name ldx-condition-))

(define-results ldx-error (com-error ldx-condition)
  ()
  ((invalid-call #x8876086C
     "The method call is invalid. For example, a method's parameter may not be a valid pointer.")
   (was-still-drawing #x8876021C
     "The previous blit operation that is transferring information to or from this surface is incomplete.")
   (file-not-found #x88790002
     "The file was not found.")
   (too-many-unique-state-objects #x88790001
     "There are too many unique instances of a particular type of state object."))
  (:conc-name ldx-error-))

(define-results ldx-status (windows-status ldx-condition)
  ()
  ()
  (:conc-name ldx-status-))
