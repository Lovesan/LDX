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

(in-package #:ldx.compiler)

(define-struct (shader-data
                 (:constructor make-shader-data)
                 (:constructor shader-data (bytecode length)))
    "Describes shader data."
  (bytecode pointer)
  (length size-t))

(define-enum (blob-part
               (:base-type dword)
               (:conc-name blob-))
  :input-signature-blob
  :output-signature-blob
  :input-and-output-signature-blob
  :patch-constant-signature-blob
  :all-signature-blob
  :debug-info
  :legacy-shader
  :xna-prepass-shader
  :xna-shader
  (:test-alternate-shader #x8000)
  :compile-details
  :compile-perf)

(define-enum (compiler-strip-flags
               (:base-type dword)
               (:list t)
               (:conc-name compiler-strip-))
  (:reflection-data 1)
  (:debug-info 2)
  (:test-blobs 4))

(define-enum (shader-compiler-flags
               (:base-type dword)
               (:conc-name compile-)
               (:list t))
  (:avoid-control-flow #.(ash 1 9))
  (:debug #.(ash 1 0))
  (:enable-backwards-compatibility #.(ash 1 12))
  (:enable-strictness #.(ash 1 11))
  (:force-ps-software-no-opt #.(ash 1 7))
  (:force-vs-software-no-opt #.(ash 1 6))
  (:ieee-strictness #.(ash 1 13))
  (:no-preshader #.(ash 1 8))
  (:optimization-level-0 #.(ash 1 14))
  (:optimization-level-1 0)
  (:optimization-level-2 #.(logior (ash 1 14)
                                   (ash 1 15)))
  (:optimization-level-3 #.(ash 1 15))
  (:pack-matrix-column-major #.(ash 1 4))
  (:pack-matrix-row-major #.(ash 1 3))
  (:partial-precision #.(ash 1 5))
  (:prefer-flow-control #.(ash 1 10))
  (:skip-optimization #.(ash 1 2))
  (:skip-validation #.(ash 1 1))
  (:warnings-are-errors #.(ash 1 18)))

(define-enum (effect-compiler-flags
               (:conc-name compile-effect-)
               (:base-type dword)
               (:list t))
  (:child-effect #.(ash 1 0))
  (:allow-slow-ops #.(ash 1 1)))

(define-enum (shader-compressor-flags
               (:conc-name compress-shader-)
               (:base-type dword)
               (:list t))
  (:keep-all-parts 1))

(define-enum (shader-disassembler-flags
               (:conc-name disasm-)
               (:base-type dword)
               (:list t))
  (:enable-color-code #x1)
  (:enable-default-value-prints #x2)
  (:enable-instruction-numbering #x4)
  (:enable-instruction-cycle #x8)
  (:disable-debug-info #x10))

(define-results shader-compiler-error (ldx-error)
  ((message :initform nil :initarg :message
            :accessor shader-compiler-error-message))
  ()
  (:report (lambda (c s)
             (declare (type stream s))
             (let ((message (shader-compiler-error-message c)))
               (if message
                 (pprint-logical-block (s nil)
                   (format s "Shader compilation error:")
                   (pprint-newline :mandatory s)
                   (with-input-from-string (in message)
                     (loop :for l = (read-line in nil nil)
                       :while l :do
                       (write-string l s)
                       (pprint-newline :mandatory s))))
                 (print-windows-condition c s))
               c)))
  (:conc-name shader-compiler-error-))

(defun compile-shader (code target entry-point
                       &key source-name
                            defines
                            include
                            shader-flags
                            effect-flags)
  "Compile HLSL code into bytecode for a given target."
  (declare (type string code target entry-point)
           (type (or null string) source-name)
           (type sequence defines)
           (type (or null include) include)
           (type shader-compiler-flags shader-flags)
           (type effect-compiler-flags effect-flags))
  (multiple-value-bind
      (hr code errors)
      (external-function-call
        "D3DCompile"
        ((:stdcall d3dcompiler)
         (dword rv (values rv code errors))
         ((& astring) data :aux code)
         (size-t size :aux (length code))
         ((& astring :in t) src :aux (or source-name void))
         ((& (~ shader-macro) :in t) defines :aux (or defines void))
         (include inc :aux include)
         ((& astring) entry :aux entry-point)
         ((& astring) tgt :aux target)
         (shader-compiler-flags flags1 :aux shader-flags)
         (effect-compiler-flags flags2 :aux effect-flags)
         ((& blob :out) code :aux)
         ((& blob :out) errors :aux)))
    (if (hresult-error-p hr)
      (error 'shader-compiler-error
             :code hr
             :message (when errors 
                        (deref (blob-pointer errors)
                               'astring)))
      (progn (translate hr 'hresult)
             code))))

(define-external-function
    ("D3DCompressShaders" compress-shaders)
    (:stdcall d3dcompiler)
  (hresult rv data)
  "Compresses a set of shaders into a more compact form."
  (num-shaders uint :aux (length shaders))
  (shaders (& (~ shader-data)))
  (flags shader-compressor-flags :key)
  (data (& blob :out) :aux))

(define-external-function
    ("D3DCreateBlob" create-blob)
    (:stdcall d3dcompiler)
  (hresult rv blob)
  "Creates a buffer"
  (size size-t)
  (blob (& blob :out) :aux))

(define-external-function
    ("D3DDecompressShaders" decompress-shaders)
    (:stdcall d3dcompiler)
  (hresult rv (loop :for i :below num :collect (svref shaders i)))
  "Decompresses one or more shaders from a compressed set."
  (data pointer)
  (size size-t)
  (num-shaders uint)
  (start uint :key)
  (indices (& (sequence uint))
           :key (loop :for i :below num-shaders :collect i))
  (flags uint :aux)
  (shaders (& (simple-array blob *) :out)
           :aux (make-array num-shaders))
  (num (& uint :out) :aux))

(define-external-function
    ("D3DDisassemble" disassemble-shader)
    (:stdcall d3dcompiler)
  (hresult rv (deref (blob-pointer blob) 'astring))
  "Disassembles compiled HLSL code."
  (data pointer)
  (size size-t)
  (flags shader-disassembler-flags :key)
  (comments (& astring) :key)
  (blob (& blob :out) :aux))

(define-external-function
    ("D3DGetBlobPart" blob-part)
    (:stdcall d3dcompiler)
  (hresult rv blob)
  "Retrieves a specific part from a compilation result."
  (data pointer)
  (size size-t)
  (part blob-part)
  (flags uint :aux)
  (blob (& blob :out) :aux))

(define-external-function
    ("D3DGetDebugInfo" debug-info)
    (:stdcall d3dcompiler)
  (hresult rv blob)
  "Get shader debug information."
  (data pointer)
  (size size-t)
  (blob (& blob :out) :aux))

(define-external-function
    ("D3DGetInputAndOutputSignatureBlob" input-and-output-signature-blob)
    (:stdcall d3dcompiler)
  (hresult rv blob)
  "Gets the input and output signatures from a compilation result."
  (data pointer)
  (size size-t)
  (blob (& blob :out) :aux))

(define-external-function
    ("D3DGetInputSignatureBlob" input-signature-blob)
    (:stdcall d3dcompiler)
  (hresult rv blob)
  "Gets the input signature from a compilation result."
  (data pointer)
  (size size-t)
  (blob (& blob :out) :aux))

(define-external-function
    ("D3DGetOutputSignatureBlob" output-signature-blob)
    (:stdcall d3dcompiler)
  (hresult rv blob)
  "Gets the output signature from a compilation result."
  (data pointer)
  (size size-t)
  (blob (& blob :out) :aux))

(define-external-function
    ("D3DStripShader" strip-shader)
    (:stdcall d3dcompiler)
  (hresult rv blob)
  "Removes blobs from a compilation result."
  (bytecode pointer)
  (bytecode-length size-t)
  (flags compiler-strip-flags :key)
  (blob (& blob :out) :aux))

(define-external-function
    ("D3DReflect" reflect-shader)
    (:stdcall d3dcompiler)
  (hresult rv (translate-interface reflector iid t))
  "Gets a pointer to a reflection interface."
  (data pointer)
  (size size-t)
  (iid (& iid))
  (reflector (& pointer :out) :aux))

(defun preprocess-shader (code &key source-name
                                    defines
                                    include)
  "Preprocesses uncompiled HLSL code."
  (declare (type string code)
           (type (or null string) source-name)
           (type sequence defines)
           (type (or null include) include))
  (multiple-value-bind
      (hr code errors)
      (external-function-call
        "D3DPreprocess"
        ((:stdcall d3dcompiler)
         (dword rv (values rv code errors))
         ((& astring) data :aux code)
         (size-t size :aux (length code))
         ((& astring :in t) src :aux (or source-name void))
         ((& (~ shader-macro) :in t) defines :aux (or defines void))
         (include inc :aux include)
         ((& blob :out) code :aux)
         ((& blob :out) errors :aux)))
    (if (hresult-error-p hr)
      (error 'shader-compiler-error
             :code hr
             :message (when errors 
                        (deref (blob-pointer errors)
                               'astring)))
      (progn (translate hr 'hresult)
             (deref (blob-pointer code) 'astring)))))
