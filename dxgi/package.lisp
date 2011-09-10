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

(defpackage #:ldx.dxgi
  (:nicknames #:dxgi)
  (:use #:cl #:alexandria #:virgil #:doors #:doors.com #:ldx)
  (:export
    
    ;;DLL
    #:dxgi
    
    ;;condition classes and codes
    #:dxgi-error
    #:dxgi-error-invalid-call
    #:dxgi-error-not-found
    #:dxgi-error-more-data
    #:dxgi-error-unsopported
    #:dxgi-error-device-removed
    #:dxgi-error-device-hung
    #:dxgi-error-device-reset
    #:dxgi-error-was-still-drawing
    #:dxgi-error-frame-statistics-disjoint
    #:dxgi-error-graphics-vidpn-source-in-use
    #:dxgi-error-internal-error
    #:dxgi-error-non-exclusive
    #:dxgi-error-not-currently-available
    #:dxgi-error-remote-client-disconnected
    #:dxgi-error-remote-out-of-memory
    #:dxgi-status
    #:dxgi-status-occluded
    #:dxgi-status-clipped
    #:dxgi-status-no-redirection
    #:dxgi-status-no-desktop-access
    #:dxgi-status-graphics-vidpn-source-in-use
    #:dxgi-status-mode-changed
    #:dxgi-status-mode-change-in-progress
    
    ;;enumerations
    #:enum-modes
    #:enum-modes-interlaces
    #:enum-modes-scaling
    #:present-flag
    #:present-do-not-sequence
    #:present-test
    #:present-restart
    #:adapter-flag
    #:adapter-flag-none
    #:adapter-flag-remote
    #:residency
    #:residency-fully-resident
    #:residency-resident-in-shared-memory
    #:residency-evicted-to-disk
    #:usage
    #:usage-shader-input
    #:usage-render-target-output
    #:usage-back-buffer
    #:usage-shared
    #:usage-read-only
    #:usage-discard-on-present
    #:usage-unordered-access
    #:swap-effect
    #:swap-effect-discard
    #:swap-effect-sequential
    #:swap-chain-flag
    #:swap-chain-flag-non-prerotated
    #:swap-chain-flag-allow-mode-switch
    #:swap-chain-flag-gdi-compatible
    #:mode-rotation
    #:mode-rotation-unspecified
    #:mode-rotation-identity
    #:mode-rotation-rotate90
    #:mode-rotation-rotate180
    #:mode-rotation-rotate270
    #:mode-scaling
    #:mode-scaling-unspecified
    #:mode-scaling-centered
    #:mode-scaling-stretched
    #:mode-scanline-order
    #:mode-scanline-order-unspecified
    #:mode-scanline-order-progressive
    #:mode-scanline-order-upper-field-first
    #:mode-scanline-order-lower-field-first
    #:resource-priority
    #:resource-priority-minimum
    #:resource-priority-low
    #:resource-priority-normal
    #:resource-priority-high
    #:resource-priority-maximum
    #:dxgi-format
    #:format-unknown
    #:format-r32g32b32a32-typeless
    #:format-r32g32b32a32-float
    #:format-r32g32b32a32-uint
    #:format-r32g32b32a32-sint
    #:format-r32g32b32-typeless
    #:format-r32g32b32-float
    #:format-r32g32b32-uint
    #:format-r32g32b32-sint
    #:format-r16g16b16a16-typeless
    #:format-r16g16b16a16-float
    #:format-r16g16b16a16-unorm
    #:format-r16g16b16a16-uint
    #:format-r16g16b16a16-snorm
    #:format-r16g16b16a16-sint
    #:format-r32g32-typeless
    #:format-r32g32-float
    #:format-r32g32-uint
    #:format-r32g32-sint
    #:format-r32g8x24-typeless
    #:format-d32-float-s8x24-uint
    #:format-r32-float-x8x24-typeless
    #:format-x32-typeless-g8x24-uint
    #:format-r10g10b10a2-typeless
    #:format-r10g10b10a2-unorm
    #:format-r10g10b10a2-uint
    #:format-r11g11b10-float
    #:format-r8g8b8a8-typeless
    #:format-r8g8b8a8-unorm
    #:format-r8g8b8a8-unorm-srgb
    #:format-r8g8b8a8-uint
    #:format-r8g8b8a8-snorm
    #:format-r8g8b8a8-sint
    #:format-r16g16-typeless
    #:format-r16g16-float
    #:format-r16g16-unorm
    #:format-r16g16-uint
    #:format-r16g16-snorm
    #:format-r16g16-sint
    #:format-r32-typeless
    #:format-d32-float
    #:format-r32-float
    #:format-r32-uint
    #:format-r32-sint
    #:format-r24g8-typeless
    #:format-d24-unorm-s8-uint
    #:format-r24-unorm-x8-typeless
    #:format-x24-typeless-g8-uint
    #:format-r8g8-typeless
    #:format-r8g8-unorm
    #:format-r8g8-uint
    #:format-r8g8-snorm
    #:format-r8g8-sint
    #:format-r16-typeless
    #:format-r16-float
    #:format-d16-unorm
    #:format-r16-unorm
    #:format-r16-uint
    #:format-r16-snorm
    #:format-r16-sint
    #:format-r8-typeless
    #:format-r8-unorm
    #:format-r8-uint
    #:format-r8-snorm
    #:format-r8-sint
    #:format-a8-unorm
    #:format-r1-unorm
    #:format-r9g9b9e5-sharedexp
    #:format-r8g8-b8g8-unorm
    #:format-g8r8-g8b8-unorm
    #:format-bc1-typeless
    #:format-bc1-unorm
    #:format-bc1-unorm-srgb
    #:format-bc2-typeless
    #:format-bc2-unorm
    #:format-bc2-unorm-srgb
    #:format-bc3-typeless
    #:format-bc3-unorm
    #:format-bc3-unorm-srgb
    #:format-bc4-typeless
    #:format-bc4-unorm
    #:format-bc4-snorm
    #:format-bc5-typeless
    #:format-bc5-unorm
    #:format-bc5-snorm
    #:format-b5g6r5-unorm
    #:format-b5g5r5a1-unorm
    #:format-b8g8r8a8-unorm
    #:format-b8g8r8x8-unorm
    #:format-r10g10b10-xr-bias-a2-unorm
    #:format-b8g8r8a8-typeless
    #:format-b8g8r8a8-unorm-srgb
    #:format-b8g8r8x8-typeless
    #:format-b8g8r8x8-unorm-srgb
    #:format-bc6h-typeless
    #:format-bc6h-uf16
    #:format-bc6h-sf16
    #:format-bc7-typeless
    #:format-bc7-unorm
    #:format-bc7-unorm-srgb
    
    ;;structures
    #:rgb
    #:dxgi-rational
    #:mode-desc
    #:make-mode-desc
    #:copy-mode-desc
    #:mode-desc-p
    #:mode-width
    #:mode-height
    #:mode-refresh-rate
    #:mode-format
    #:mode-scanline-order
    #:mode-scaling
    #:sample-desc
    #:make-sample-desc
    #:copy-sample-desc
    #:sample-desc-p
    #:sample-count
    #:sample-quality
    #:shared-resource
    #:make-shared-resource
    #:shared-resource-p
    #:shared-resource-handle
    #:output-desc
    #:make-output-desc
    #:copy-output-desc
    #:output-desc-p
    #:output-device-name
    #:output-desktop-coordinates
    #:output-attached-to-desktop
    #:output-rotation
    #:output-monitor
    #:surface-desc
    #:make-surface-desc
    #:copy-surface-desc
    #:surface-desc-p
    #:surface-width
    #:surface-height
    #:surface-format
    #:surface-sample-desc
    #:swap-chain-desc
    #:make-swap-chain-desc
    #:copy-swap-chain-desc
    #:swap-chain-desc-p
    #:swap-chain-buffer-desc
    #:swap-chain-sample-desc
    #:swap-chain-buffer-usage
    #:swap-chain-buffer-count
    #:swap-chain-output-window
    #:swap-chain-windowed
    #:swap-chain-swap-effect
    #:swap-chain-flags
    #:mapped-rect
    #:make-mapped-rect    
    #:mapped-rect-p
    #:mapped-rect-pitch
    #:mapped-rect-bits
    #:adapter-desc
    #:make-adapter-desc
    #:copy-adapter-desc
    #:adapter-desc-p
    #:adapter-description
    #:adapter-vendor-id
    #:adapter-device-id
    #:adapter-subsys-id
    #:adapter-revision
    #:adapter-dedicated-video-memory
    #:adapter-dedicated-system-memory
    #:adapter-shared-system-memory
    #:adapter-luid
    #:adapter-desc*
    #:make-adapter-desc*
    #:copy-adapter-desc*
    #:adapter-desc-p*
    #:adapter-flags
    #:frame-statistics
    #:make-frame-statistics
    #:copy-frame-statistics
    #:frame-statistics-p
    #:frame-present-count
    #:frame-present-refresh-count
    #:frame-sync-refresh-count
    #:frame-sync-qpc-time
    #:frame-sync-gpu-time
    #:gamma-control
    #:make-gamma-control
    #:copy-gamma-control
    #:gamma-control-p
    #:gamma-control-scale
    #:gamma-control-offset
    #:gamma-control-gamma-curve
    #:gamma-control-capabilities
    #:make-gamma-caps
    #:gamma-caps-p
    #:copy-gamma-caps
    #:gamma-caps-scale-and-offset-supported
    #:gamma-caps-max-converted-value
    #:gamma-caps-min-converted-value
    #:gamma-caps-num-gamma-control-points
    #:gamma-caps-control-point-positions
    
    ;;interfaces
    #:object
    #:iid-object
    #:private-data
    #:private-data-interface
    #:parent
    
    #:device-sub-object
    #:iid-device-sub-object
    #:get-device
    
    #:keyed-mutex
    #:iid-keyed-mutex
    #:acquire-sync
    #:release-sync
    
    #:resource
    #:iid-resource
    #:shared-handle
    #:eviction-priority
    
    #:surface
    #:iid-surface
    #:description
    #:map-resource
    #:unmap-resource
    
    #:surface*
    #:iid-surface*
    #:get-dc
    #:release-dc
    
    #:output
    #:iid-output
    #:display-mode-list
    #:find-closest-matching-mode
    #:wait-for-vblank
    #:take-ownership
    #:release-ownership
    #:display-surface
    #:display-surface-data
    
    #:adapter
    #:iid-adapter
    #:enum-outputs
    #:check-interface-support
    
    #:adapter*
    #:iid-adapter*
    #:description*
    
    #:swap-chain
    #:iid-swap-chain
    #:present
    #:get-buffer
    #:fullscreen-state
    #:resize-buffers
    #:resize-target
    #:containing-output
    #:last-present-count
    
    #:factory
    #:iid-factory
    #:enum-adapters
    #:make-window-association
    #:window-association
    #:create-swap-chain
    #:create-software-adapter
    
    #:factory*
    #:iid-factory*
    #:enum-adapters*
    #:is-current
    
    #:create-dxgi-factory
    #:create-dxgi-factory*
    
    #:device
    #:iid-device
    #:get-adapter
    #:create-surface
    #:resource-residency
    #:gpu-thread-priority
    
    #:device*
    #:iid-device*
    #:maximum-frame-latency))
