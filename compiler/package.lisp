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

(in-package #:cl-user)

(defpackage #:ldx.compiler
  (:use #:cl #:alexandria #:virgil #:doors #:doors.com #:ldx)
  (:export
    
    ;;d3dcompiler_43.dll library
    #:d3dcompiler
    
    ;;conditions
    #:shader-compiler-error
    #:shader-compiler-error-message
    
    ;;enumerations
    #:blob-part
    #:blob-input-signature-blob
    #:blob-output-signature-blob
    #:blob-input-and-output-signature-blob
    #:blob-patch-constant-signature-blob
    #:blob-all-signature-blob
    #:blob-debug-info
    #:blob-legacy-shader
    #:blob-xna-prepass-shader
    #:blob-xna-shader
    #:blob-test-alternate-shader
    #:blob-compile-details
    #:blob-compile-perf
    #:compiler-strip-flags
    #:compiler-strip-reflection-data
    #:compiler-strip-debug-info
    #:compiler-strip-test-blobs
    #:shader-compiler-flags
    #:compile-avoid-control-flow
    #:compile-debug
    #:compile-enable-backwards-compatibility
    #:compile-enable-strictness
    #:compile-force-ps-software-no-opt
    #:compile-force-vs-software-no-opt
    #:compile-ieee-strictness
    #:compile-no-preshader
    #:compile-optimization-level-0
    #:compile-optimization-level-1
    #:compile-optimization-level-2
    #:compile-optimization-level-3
    #:compile-pack-matrix-column-major
    #:compile-pack-matrix-row-major
    #:compile-partial-precision
    #:compile-prefer-control-flow
    #:compile-skip-optimization
    #:compile-skip-validation
    #:compile-warnings-are-errors
    #:effect-compiler-flags
    #:compile-effect-child-effect
    #:compile-effect-allow-slow-ops
    #:shader-compressor-flags
    #:compress-shader-keep-all-parts
    #:shader-disassembler-flags
    #:disasm-enable-color-code
    #:disasm-enable-default-value-prints
    #:disasm-enable-instruction-numbering
    #:disasm-enable-instruction-cycle
    #:disasm-disable-debug-info
    
    ;;structures
    #:shader-data
    #:make-shader-data
    #:copy-shader-data
    #:shader-data-bytecode
    #:shader-data-length
    
    ;;functions
    #:compile-shader
    #:compress-shaders
    #:create-blob
    #:decompress-shaders
    #:disassemble-shader
    #:blob-part
    #:debug-info
    #:input-signature-blob
    #:output-signature-blob
    #:input-and-output-signature-blob
    #:preprocess-shader
    #:reflect-shader
    #:strip-shader
    ))
