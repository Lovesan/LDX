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

(define-enum (enum-modes
               (:base-type dword))
  (:interlaced 1)
  (:scaling    2))

(define-enum (present-flag
               (:conc-name present-)
               (:base-type dword))
  (:do-not-sequence 2)
  (:test 1)
  (:restart 4))

(define-enum (adapter-flag
               (:base-type dword))
  (:none 0)
  (:remote 1))

(define-enum (residency
               (:base-type dword))
  (:fully-resident 1)
  (:resident-in-shared-memory 2)
  (:evicted-to-disk 3))

(define-enum (usage               
               (:base-type dword))
  (:shader-input            #.(ash 1 (+ 0 4)))
  (:render-target-output    #.(ash 1 (+ 1 4)))
  (:back-buffer             #.(ash 1 (+ 2 4)))
  (:shared                  #.(ash 1 (+ 3 4)))
  (:read-only               #.(ash 1 (+ 4 4)))
  (:discard-on-present      #.(ash 1 (+ 5 4)))
  (:unordered-access        #.(ash 1 (+ 6 4))))

(define-enum (swap-effect              
               (:base-type dword))
  (:discard 0)
  (:sequential 1))

(define-enum (swap-chain-flag
               (:list t)
               (:base-type dword))
  (:non-prerotated 1)
  (:allow-mode-switch 2)
  (:gdi-compatible 4))

(define-enum (mode-rotation
               (:base-type dword))
  :unspecified
  :identity
  :rotate90
  :rotate180
  :rotate270)

(define-enum (mode-scaling
               (:base-type dword))
  :unspecified
  :centered
  :stretched)

(define-enum (mode-scanline-order
               (:base-type dword))
  :unspecified
  :progressive
  :upper-field-first
  :lower-field-first)

(define-enum (resource-priority
               (:base-type uint))
  (:minimum #x28000000)
  (:low #x50000000)
  (:normal #x78000000)
  (:high #xa0000000)
  (:maximum #xc8000000))

(define-enum (dxgi-format
               (:base-type dword)
               (:conc-name format-))
  (:UNKNOWN                       0)
  (:R32G32B32A32-TYPELESS         1)
  (:R32G32B32A32-FLOAT            2)
  (:R32G32B32A32-UINT             3)
  (:R32G32B32A32-SINT             4)
  (:R32G32B32-TYPELESS            5)
  (:R32G32B32-FLOAT               6)
  (:R32G32B32-UINT                7)
  (:R32G32B32-SINT                8)
  (:R16G16B16A16-TYPELESS         9)
  (:R16G16B16A16-FLOAT            10)
  (:R16G16B16A16-UNORM            11)
  (:R16G16B16A16-UINT             12)
  (:R16G16B16A16-SNORM            13)
  (:R16G16B16A16-SINT             14)
  (:R32G32-TYPELESS               15)
  (:R32G32-FLOAT                  16)
  (:R32G32-UINT                   17)
  (:R32G32-SINT                   18)
  (:R32G8X24-TYPELESS             19)
  (:D32-FLOAT-S8X24-UINT          20)
  (:R32-FLOAT-X8X24-TYPELESS      21)
  (:X32-TYPELESS-G8X24-UINT       22)
  (:R10G10B10A2-TYPELESS          23)
  (:R10G10B10A2-UNORM             24)
  (:R10G10B10A2-UINT              25)
  (:R11G11B10-FLOAT               26)
  (:R8G8B8A8-TYPELESS             27)
  (:R8G8B8A8-UNORM                28)
  (:R8G8B8A8-UNORM-SRGB           29)
  (:R8G8B8A8-UINT                 30)
  (:R8G8B8A8-SNORM                31)
  (:R8G8B8A8-SINT                 32)
  (:R16G16-TYPELESS               33)
  (:R16G16-FLOAT                  34)
  (:R16G16-UNORM                  35)
  (:R16G16-UINT                   36)
  (:R16G16-SNORM                  37)
  (:R16G16-SINT                   38)
  (:R32-TYPELESS                  39)
  (:D32-FLOAT                     40)
  (:R32-FLOAT                     41)
  (:R32-UINT                      42)
  (:R32-SINT                      43)
  (:R24G8-TYPELESS                44)
  (:D24-UNORM-S8-UINT             45)
  (:R24-UNORM-X8-TYPELESS         46)
  (:X24-TYPELESS-G8-UINT          47)
  (:R8G8-TYPELESS                 48)
  (:R8G8-UNORM                    49)
  (:R8G8-UINT                     50)
  (:R8G8-SNORM                    51)
  (:R8G8-SINT                     52)
  (:R16-TYPELESS                  53)
  (:R16-FLOAT                     54)
  (:D16-UNORM                     55)
  (:R16-UNORM                     56)
  (:R16-UINT                      57)
  (:R16-SNORM                     58)
  (:R16-SINT                      59)
  (:R8-TYPELESS                   60)
  (:R8-UNORM                      61)
  (:R8-UINT                       62)
  (:R8-SNORM                      63)
  (:R8-SINT                       64)
  (:A8-UNORM                      65)
  (:R1-UNORM                      66)
  (:R9G9B9E5-SHAREDEXP            67)
  (:R8G8-B8G8-UNORM               68)
  (:G8R8-G8B8-UNORM               69)
  (:BC1-TYPELESS                  70)
  (:BC1-UNORM                     71)
  (:BC1-UNORM-SRGB                72)
  (:BC2-TYPELESS                  73)
  (:BC2-UNORM                     74)
  (:BC2-UNORM-SRGB                75)
  (:BC3-TYPELESS                  76)
  (:BC3-UNORM                     77)
  (:BC3-UNORM-SRGB                78)
  (:BC4-TYPELESS                  79)
  (:BC4-UNORM                     80)
  (:BC4-SNORM                     81)
  (:BC5-TYPELESS                  82)
  (:BC5-UNORM                     83)
  (:BC5-SNORM                     84)
  (:B5G6R5-UNORM                  85)
  (:B5G5R5A1-UNORM                86)
  (:B8G8R8A8-UNORM                87)
  (:B8G8R8X8-UNORM                88)
  (:R10G10B10-XR-BIAS-A2-UNORM    89)
  (:B8G8R8A8-TYPELESS             90)
  (:B8G8R8A8-UNORM-SRGB           91)
  (:B8G8R8X8-TYPELESS             92)
  (:B8G8R8X8-UNORM-SRGB           93)
  (:BC6H-TYPELESS                 94)
  (:BC6H-UF16                     95)
  (:BC6H-SF16                     96)
  (:BC7-TYPELESS                  97)
  (:BC7-UNORM                     98)
  (:BC7-UNORM-SRGB                99))

(defalias rgb () '(simple-array single-float (3)))
(deftype rgb () '(simple-array single-float (3)))

(define-immediate-type rational-type ()
  ()
  (:simple-parser rational)
  (:base-type qword)
  (:lisp-type (type) 'rational)
  (:prototype (type) 0)
  (:prototype-expansion (type) 0)
  (:cleaner-expansion (pointer value type) nil)
  (:allocator-expansion (value type) `(alloc 'qword))
  (:deallocator-expansion (pointer type) `(free ,pointer))
  (:translator (value type)
    (/ (ldb (byte 32 32) value)
       (ldb (byte 32 0)  value)))
  (:converter (value type)
    (make-qword (numerator value) (denominator value)))
  (:translator-expansion (value type)
    (once-only ((value `(the qword ,value)))
      `(/ (ldb (byte 32 32) ,value)
          (ldb (byte 32 0)  ,value))))
  (:converter-expansion (value type)
    (once-only ((value `(the rational ,value)))
      `(make-qword (numerator ,value) (denominator ,value)))))

(defmethod compute-alignment ((type rational-type))
  (alignof 'uint))

(define-struct (mode-desc
                 (:conc-name mode-))
    "Describes a display mode."
  (width uint)
  (height uint)
  (refresh-rate rational)
  (format dxgi-format)
  (scanline-order mode-scanline-order)
  (scaling mode-scaling))

(define-struct (sample-desc
                 (:conc-name sample-))
    "Describes multi-sampling parameters for a resource."
  (count uint :initform 1)
  (quality (enum (:base-type uint)
             (:standard-multisample-pattern #xFFFFFFFF)
             (:center-multisample-pattern #xFFFFFFFE))))

(define-struct (shared-resource
                 (:constructor make-shared-resource)
                 (:constructor shared-resource (handle)))
    "Represents a handle to a shared resource."
  (handle handle))

(define-struct (output-desc
                 (:conc-name output-))
    "Describes an output or physical connection between the adapter (video card) and a device."
  (device-name (wstring 32))
  (desktop-coordinates doors.gdi:rect)
  (attached-to-desktop boolean)
  (rotation mode-rotation)
  (monitor handle))

(define-struct (surface-desc
                 (:conc-name surface-))
    "Describes a surface."
  (width uint)
  (height uint)
  (format dxgi-format)
  (sample-desc sample-desc))

(define-struct (swap-chain-desc
                 (:conc-name swap-chain-))
    "Describes a swap chain."
  (buffer-desc mode-desc)
  (sample-desc sample-desc)
  (buffer-usage usage)
  (buffer-count uint)
  (output-window handle)
  (windowed boolean)
  (swap-effect swap-effect)
  (flags swap-chain-flag))

(define-struct (mapped-rect)
    "A mapped rectangle used for accessing a surface."
  (pitch int)
  (bits pointer))

(define-struct (adapter-desc
                 (:conc-name adapter-))
    "Describes an adapter (or video card) by using DXGI 1.0."
  (description (wstring 128))
  (vendor-id uint)
  (device-id uint)
  (subsys-id uint)
  (revision uint)
  (dedicated-video-memory size-t)
  (dedicated-system-memory size-t)
  (shared-system-memory size-t)
  (luid int64))

(define-struct (adapter-desc*
                 (:predicate adapter-desc-p*)
                 (:include adapter-desc)
                 (:conc-name adapter-))
    "Describes an adapter (or video card) using DXGI 1.1."
  (flags adapter-flag))

(define-struct (frame-statistics
                 (:conc-name frame-))
    "Describes timing and presentation statistics for a frame."
  (present-count uint)
  (present-refresh-count uint)
  (sync-refresh-count uint)
  (sync-qpc-time int64)
  (sync-gpu-time int64))

(define-struct (gamma-control)
    "Controls the settings of a gamma curve."
  (scale rgb)
  (offset rgb)
  (gamma-curve (simple-array rgb (1025))))

(define-struct (gamma-control-capabilities
                 (:constructor make-gamma-caps)
                 (:conc-name gamma-caps-)
                 (:predicate gamma-caps-p)
                 (:copier copy-gamma-caps))
    "Controls the gamma capabilities of an adapter."
  (scale-and-offset-supported boolean)
  (max-converted-value float)
  (min-converted-value float)
  (num-gamma-control-points uint)
  (control-point-positions (simple-array float (1025))))
