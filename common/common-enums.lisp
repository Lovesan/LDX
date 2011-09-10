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

(define-enum (cbuffer-type
               (:base-type dword)
               (:conc-name ct-))
  :cbuffer
  :tbuffer
  :interface-pointers
  :resource-bind-info)

(define-enum (feature-level
               (:base-type dword)
               (:list t)
               (:conc-name feature-level))
  (:9.1  #x9100)
  (:9.2  #x9200)
  (:9.3  #x9300)
  (:10.0 #xA000)
  (:10.1 #xA100)
  (:11.1 #xB000))

(define-enum (include-type
               (:base-type dword)
               (:conc-name include-))
  :local
  :system)

(define-enum (name
               (:base-type dword)
               (:conc-name name-))
  :undefined
  :position
  :clip-distance
  :cull-distance
  :render-target-array-index
  :viewport-array-index
  :vertex-id
  :primitive-id
  :instance-id
  :is-front-face
  :sample-index
  :final-quad-edge-tessfactor
  :final-quad-inside-tessfactor
  :final-tri-edge-tessfactor
  :final-tri-inside-tessfactor
  :final-line-detail-tessfactor
  :final-line-density-tessfactor
  (:target 64)
  :depth
  :coverage
  :depth-greater-equal
  :depth-less-equal)

(define-enum (primitive
               (:conc-name primitive-)
               (:base-type dword))
  :undefined
  :point
  :line
  :triangle
  :line-adj
  :triangle-adj
  :1-control-point-patch
  :2-control-point-patch
  :3-control-point-patch
  :4-control-point-patch
  :5-control-point-patch
  :6-control-point-patch
  :7-control-point-patch
  :8-control-point-patch
  :9-control-point-patch
  :10-control-point-patch
  :11-control-point-patch
  :12-control-point-patch
  :13-control-point-patch
  :14-control-point-patch
  :15-control-point-patch
  :16-control-point-patch
  :17-control-point-patch
  :18-control-point-patch
  :19-control-point-patch
  (:20-control-point-patch 28)
  :21-control-point-patch
  :22-control-point-patch
  :23-control-point-patch
  :24-control-point-patch
  :25-control-point-patch
  :26-control-point-patch
  :27-control-point-patch
  :28-control-point-patch
  :29-control-point-patch
  :30-control-point-patch
  :31-control-point-patch
  :32-control-point-patch)

(define-enum (primitive-topology
               (:conc-name primitive-topology-)
               (:base-type dword))
  :undefined
  :point-list
  :line-list
  :line-strip
  :triangle-list
  :triangle-strip
  (:line-list-adj 10)
  :line-strip-adj
  :triangle-list-adj
  :triangle-strip-adj
  (:1-control-point-patch-list 33)
  :2-control-point-patch-list
  :3-control-point-patch-list
  :4-control-point-patch-list
  :5-control-point-patch-list
  :6-control-point-patch-list
  :7-control-point-patch-list
  :8-control-point-patch-list
  :9-control-point-patch-list
  :10-control-point-patch-list
  :11-control-point-patch-list
  :12-control-point-patch-list
  :13-control-point-patch-list
  :14-control-point-patch-list
  :15-control-point-patch-list
  :16-control-point-patch-list
  :17-control-point-patch-list
  :18-control-point-patch-list
  :19-control-point-patch-list
  :20-control-point-patch-list
  :21-control-point-patch-list
  :22-control-point-patch-list
  :23-control-point-patch-list
  :24-control-point-patch-list
  :25-control-point-patch-list
  :26-control-point-patch-list
  :27-control-point-patch-list
  :28-control-point-patch-list
  :29-control-point-patch-list
  :30-control-point-patch-list
  :31-control-point-patch-list
  :32-control-point-patch-list)

(define-enum (register-component-type
               (:conc-name register-component-)
               (:base-type dword))
  :unknown
  :uint32
  :sint32
  :float32)

(define-enum (resource-return-type
               (:conc-name return-type-)
               (:base-type dword))
  (:unorm 1)
  :snorm
  :sint
  :uint
  :float
  :mixed
  :double
  :continued)

(define-enum (shader-cbuffer-flags
               (:conc-name cbf-)
               (:list t)
               (:base-type dword))
  (:user-packed 1))
  
(define-enum (shader-input-flags
               (:conc-name sif-)
               (:list t)
               (:base-type dword))
  (:user-packed 1)
  (:comparison-sampler 2)
  (:texture-component-0 4)
  (:texture-component-1 8)
  (:texture-components 12))

(define-enum (shader-input-type
               (:conc-name sit-)
               (:base-type dword))
  :cbuffer
  :tbuffer
  :texture
  :sampler
  :uav-rw-typed
  :structured
  :uav-rw-structured
  :byte-address
  :uav-rw-byte-address
  :uav-append-structured
  :uav-consume-structured
  :uav-rw-structured-with-counter)

(define-enum (shader-variable-class
               (:base-type dword)
               (:conc-name svc-))
  :scalar
  :vector
  :matrix-rows
  :matrix-columns
  :object
  :struct
  :interface-class
  :interface-pointer)

(define-enum (shader-variable-flags
               (:base-type dword)
               (:conc-name svf-)
               (:list t))
  (:user-packed 1)
  (:used 2)
  (:interface-pointer 4)
  (:interface-parameter 8))

(define-enum (shader-variable-type
               (:base-type dword)
               (:conc-name svt-))
  :void
  :bool
  :int
  :float
  :string
  :texture
  :texture-1d
  :texture-2d
  :texture-3d
  :texture-cube
  (:sampler 10)
  (:pixel-shader 15)
  :vertex-shader
  (:uint 19)
  :uint8
  :geometry-shader
  :rasterizer
  :depth-stencil
  :blend
  :buffer
  :cbuffer
  :tbuffer
  :texture-1d-array
  :texture-2d-array
  :render-target-view
  :depth-stencil-view
  :texture-2d-ms
  :texture-2d-ms-array
  :texture-cube-array
  :hull-shader
  :domain-shader
  :interface-pointer
  :compute-shader
  :double
  :rw-texture-1d
  :rw-texture-1d-array
  :rw-texture-2d
  :rw-texture-2d-array
  :rw-texture-3d
  :rw-buffer
  :byte-address-buffer
  :rw-byte-address-buffer
  :structured-buffer
  :rw-structured-buffer
  :append-structured-buffer
  :consume-structured-buffer)

(define-enum (resource-dimension
               (:base-type dword)
               (:conc-name resource-dimension-))
  :unknown
  :buffer
  :texture-1d
  :texture-2d
  :texture-3d)

(define-enum (srv-dimension
               (:base-type dword)
               (:conc-name srv-dimension-))
  :unknown
  :buffer
  :texture-1d
  :texture-1d-array
  :texture-2d
  :texture-2d-array
  :texture-2d-ms
  :texture-2d-ms-array
  :texture-3d
  :texture-cube
  :texture-cube-array
  :buffer*)

(define-enum (dsv-dimension
               (:base-type dword)
               (:conc-name dsv-dimension-))
  :unknown
  :texture-1d
  :texture-1d-array
  :texture-2d
  :texture-2d-array
  :texture-2d-ms
  :texture-2d-ms-array)

(define-enum (rtv-dimension
               (:base-type dword)
               (:conc-name rtv-dimension))
  :undefined
  :buffer
  :texture-1d
  :texture-1d-array
  :texture-2d
  :texture-2d-array
  :texture-2d-ms
  :texture-2d-ms-array
  :texture-3d)

(define-enum (tesselator-domain
               (:base-type dword)
               (:conc-name tesselator-domain-))
  :undefined
  :isoline
  :tri
  :quad)

(define-enum (tesselator-output-primitive
               (:conc-name tesselator-output-)
               (:base-type dword))
  :undefined
  :point
  :line
  :triangle-cw
  :triangle-ccw)

(define-enum (tesselator-partitioning
               (:base-type dword)
               (:conc-name tesselator-partitioning-))
  :undefined
  :integer
  :pow2
  :fractional-odd
  :fractional-even)

(define-enum (shader-version-type
               (:conc-name shver-)
               (:base-type dword))
  :pixel-shader
  :vertex-shader
  :geometry-shader
  :hull-shader
  :domain-shader
  :compute-shader)

(define-enum (bind-flags
               (:base-type dword)
               (:list t)
               (:conc-name bind-))
  (:vertex-buffer #x01)
  (:index-buffer #x02)
  (:constant-buffer #x04)
  (:shader-resource #x08)
  (:stream-output #x10)
  (:render-target #x20)
  (:depth-stencil #x40)
  (:unordered-access #x80))

(define-enum (buffer-srv-flags
               (:base-type dword)
               (:list t)
               (:conc-name buffer-srv-flag-))
  (:raw #x1))

(define-enum (buffer-uav-flags
               (:base-type dword)
               (:list t)
               (:conc-name buffer-uav-flag-))
  (:raw #x1)
  (:append #x2)
  (:counter #x4))

(define-enum (cpu-access-flags
               (:base-type dword)
               (:list t)
               (:conc-name cput-access-))
  (:write #x10000)
  (:read #x10000))

(define-enum (dsv-flags
               (:base-type dword)
               (:conc-name dsv-)
               (:list t))
  (:read-only-depth #x1)
  (:read-only-stencil #x2))

(define-enum (map-type
               (:base-type dword)
               (:conc-name map-))
  (:read 1)
  :write
  :read-write
  :write-discard
  :write-no-overwrite)

(define-enum (map-flags
               (:base-type dword)
               (:conc-name map-flag-)
               (:list t))
  (:do-not-wait #x100000))

(define-enum (standard-multisample-quality-levels
               (:conc-name nil)
               (:base-type dword))
  (:standard-multisample-pattern #xFFFFFFFF)
  (:center-multisample-pattern #xFFFFFFFE))

(define-enum (usage
               (:conc-name usage-)
               (:base-type dword))
  :default
  :immutable
  :dynamic
  :staging)

(define-enum (async-get-data-flags
               (:conc-name async-get-data-)
               (:base-type dword)
               (:list t))
  (:do-not-flush #x1))

(define-enum (blend-type
               (:base-type dword)
               (:conc-name blend-))
  (:zero 1)
  :one
  :src-color
  :inv-src-color
  :src-alpha
  :inv-src-alpha
  :dest-alpha
  :inv-dest-alpha
  :dest-color
  :inv-dest-color
  :src-alpha-sat
  (:blend-factor 14)
  :inv-blend-factor
  :src1-color
  :inv-src1-color
  :src1-alpha
  :inv-src1-alpha)

(define-enum (blend-op
               (:conc-name blend-op-)
               (:base-type dword))
  (:add 1)
  :subtract
  :rev-subtract
  :min
  :max)

(define-enum (clear-flags
               (:base-type dword)
               (:conc-name clear-)
               (:list t))
  (:depth #x1)
  (:stencil #x2))

(define-enum (color-write-enable
               (:conc-name color-write-enable-)
               (:base-type dword)
               (:list t))
  (:red 1)
  (:green 2)
  (:blue 4)
  (:alpha 8)
  (:all #xF))

(define-enum (comparison-func
               (:conc-name comparison-)
               (:base-type dword))
  (:never 1)
  :less
  :equal
  :less-equal
  :greater
  :not-equal
  :greater-equal
  :always)

(define-enum (counter-class
               (:conc-name counter-)
               (:base-type dword))
  :gpu-idle
  :vertex-processing
  :geometry-processing
  :pixel-processing
  :other-gpu-processing
  :host-adapter-bandwidth-utilization
  :local-vidmem-bandwidth-utilization
  :vertex-throughput-utilization
  :triangle-setup-throughput-utilization
  :fillrate-throughput-utilization
  :vs-memory-limited
  :vs-computation-limited
  :gs-memory-limited
  :gs-computation-limited
  :ps-memory-limited
  :ps-computation-limited
  :post-transform-cache-hit-rate
  :texture-cache-hit-rate
  (:device-dependent #x40000000))

(define-enum (counter-type
               (:base-type dword)
               (:conc-name counter-type-))
  :float32
  :uint16
  :uint32
  :uint64)

(define-enum (create-device-flags
               (:base-type dword)
               (:conc-name create-device-)
               (:list t))
  (:single-threaded #x1)
  (:debug #x2)
  (:switch-to-ref #x4)
  (:prevent-internal-threading-optimizations #x8)
  (:allow-null-from-map #x10)
  (:bgra-support #x20)
  (:strict-validation #x200))

(define-enum (cull-mode
               (:conc-name cull-)
               (:base-type dword))
  (:none 1)
  :front
  :back)

(define-enum (depth-write-mask
               (:conc-name depth-write-mask-)
               (:base-type dword))
  :zero
  :all)

(define-enum (device-context-type
               (:base-type dword)
               (:conc-name device-context-))
  :immediate
  :deferred)

(define-enum (feature-type
               (:conc-name feature-)
               (:base-type dword))
  :threading
  :doubles
  :format-support
  :format-support*
  :d3d10-x-hardware-options)

(define-enum (fill-mode
               (:base-type dword)
               (:conc-name fill-))
  (:wireframe 2)
  :solid)

(define-enum (filter-class
               (:conc-name filter-)
               (:base-type dword))
  (:min-mag-mip-point #x0)
  (:min-mag-point-mip-linear #x1)
  (:min-point-mag-linear-mip-point #x4)
  (:min-point-mag-mip-linear #x5)
  (:min-linear-mag-mip-point #x10)
  (:min-linear-mag-point-mip-linear #x11)
  (:min-mag-linear-mip-point #x14)
  (:min-mag-mip-linear #x15)
  (:anisotropic #x55)
  (:comparison-min-mag-mip-point #x80)
  (:comparison-min-mag-point-mip-linear #x81)
  (:comparison-min-point-mag-linear-mip-point #x84)
  (:comparison-min-point-mag-mip-linear #x85)
  (:comparison-min-linear-mag-mip-point #x90)
  (:comparison-min-linear-mag-point-mip-linear #x91)
  (:comparison-min-mag-linear-mip-point #x94)
  (:comparison-min-mag-mip-linear #x95)
  (:comparison-anisotropic #xd5)
  (:text-1bit #x80000000))

(define-enum (filter-type
               (:base-type dword)
               (:conc-name filter-type-))
  :point
  :linear)

(define-enum (format-support
               (:conc-name format-support-)
               (:base-type dword)
               (:list t))
  (:buffer #x1)
  (:ia-vertex-buffer #x2)
  (:ia-index-buffer #x4)
  (:so-buffer #x8)
  (:texture-1d #x10)
  (:texture-2d #x20)
  (:texture-3d #x40)
  (:texture-cube #x80)
  (:shader-load #x100)
  (:shader-sample #x200)
  (:shader-sample-comparison #x400)
  (:shader-sample-mono-text #x800)
  (:mip #x1000)
  (:mip-autogen #x2000)
  (:render-target #x4000)
  (:blendable #x8000)
  (:depth-stencil #x10000)
  (:cpu-lockable #x20000)
  (:multisample-resolve #x40000)
  (:display #x80000)
  (:cast-within-bit-layout #x100000)
  (:multisample-render-target #x200000)
  (:multisample-load #x400000)
  (:shader-gather #x800000)
  (:back-buffer-cast #x1000000)
  (:typed-unordered-access-view #x2000000)
  (:shader-gather-comparison #x4000000))

(define-enum (format-support*
               (:conc-name format-support-)
               (:base-type dword)
               (:list t))
  (:uav-atomic-add #x1)
  (:uav-atomic-bitwise-ops #x2)
  (:uav-atomic-compare-atore-or-compare-exchange #x4)
  (:uav-atomic-exchange #x8)
  (:uav-atomic-signed-min-or-max #x10)
  (:uav-atomic-unsigned-min-or-max #x20)
  (:uav-typed-load #x40)
  (:uav-typed-store #x80))

(define-enum (input-classification
               (:conc-name input-)
               (:base-type dword))
  :per-vertex-data
  :per-instance-data)

(define-enum (query-type
               (:conc-name query-)
               (:base-type dword))
  :event
  :occlusion
  :timestamp
  :timestamp-disjoint
  :pipeline-statistics
  :occlusion-predicate
  :so-statistics
  :so-overflow-predicate
  :so-statistics-stream-0
  :so-overflow-predicate-stream-0
  :so-statistics-stream-1
  :so-overflow-predicate-stream-1
  :so-statistics-stream-2
  :so-overflow-predicate-stream-2
  :so-statistics-stream-3
  :so-overflow-predicate-stream-3)

(define-enum (query-misc-flags
               (:conc-name query-misc-)
               (:list t)
               (:base-type dword))
  (:predicate-hint #x1))

(define-enum (raise-flags
               (:conc-name raise-flag-)
               (:base-type dword)
               (:list t))
  (:driver-internal-error #x1))

(define-enum (stencil-op
               (:base-type dword)
               (:conc-name stencil-op-))
  (:keep 1)
  :zero
  :replace
  :incr-sat
  :decr-sat
  :invert
  :incr
  :decr)

(define-enum (texture-cube-face
               (:conc-name texture-cube-face-)
               (:base-type dword))
  :positive-x
  :negative-x
  :positive-y
  :negative-y
  :positive-z
  :negative-z)

(define-enum (texture-address-mode
               (:conc-name texture-address-)
               (:base-type dword))
  (:wrap 1)
  :mirror
  :clamp
  :border
  :mirror-once)
