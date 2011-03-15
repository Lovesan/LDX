;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010-2011, Dmitry Ignatiev <lovesan.ru@gmail.com>

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

(in-package #:ldx.dxgi)

(define-results dxgi-error (com-error)
  ()
  ((invalid-call #x887A0001
"
The application provided invalid parameter data;
  this must be debugged and fixed before the application is released.")
   (not-found #x887A0002
"
When calling dxgi:private-data - the GUID passed in is not recognized as one
  previously passed to (setf dxgi:private-data) or (setf dxgi:private-data-interface).
When calling dxgi:enum-adapters or dxgi:enum-outputs, the enumerated ordinal is out of range.")
   (more-data #x887A0003
     "The buffer supplied by the application is not big enough to hold the requested data.")
   (unsupported #x887A0004
     "The requested functionality is not supported by the device or the driver.")
   (device-removed #x887A0005
     "The video card has been physically removed from the system, or a driver upgrade for the video card has occurred.")
   (device-hung #x887A0006
     "The application's device failed due to badly formed commands sent by the application.")
   (device-reset #x887A0007
"
The device failed due to a badly formed command. This is a run-time issue;
  The application should destroy and recreate the device.")
   (was-still-drawing #x887A000A
     "The device was busy, and did not schedule the requested task.")
   (frame-statistics-disjoint #x887A000B
     "The requested functionality is not supported by the device or the driver.")
   (graphics-vidpn-source-in-use #x887A000C
     "The requested functionality is not supported by the device or the driver.")
   (driver-internal-error #x887A0020
     "The driver encountered a problem and was put into the device removed state.")
   (non-exclusive #x887A0021
"
The application attempted to acquire exclusive ownership of an output,
  but failed because some other application (or device within the application) has already acquired ownership.")
   (not-currently-available #x887A0022
     "The requested functionality is not supported by the device or the driver.")
   (remote-client-disconnected #x887A0023)
   (remote-out-of-memory #x887A0024))
  (:conc-name dxgi-error-))

(define-results dxgi-status (windows-status)
  ()
  ((occluded #x087A0001
     "The window content is not visible.")
   (clipped #x087A0002)
   (no-redirection #x087A0004)
   (no-desktop-access #x087A0005)
   (graphics-vidpn-source-in-use #x087A0006)
   (mode-changed #x087A0007
     "The desktop display mode has been changed, there might be color conversion/stretching.")
   (mode-change-in-progress #x087A0008
     "Fullscreen/windowed mode transition is occurring when either API is called."))
  (:conc-name dxgi-status-))
