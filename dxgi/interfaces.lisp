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

(define-interface object
    ((iid-object "{aec22fb8-76f3-4639-9be0-28eb43a67a2e}") unknown)
  "A dxgi:object interface is a base interface for all DXGI objects"
  ((setf private-data)
     (hresult rv data)
   "Sets an IUnknown interface as private data; this associates application-defined data with the object."
   (name (& guid) :optional)
   (data-size uint :optional)
   (data pointer))
  ((setf private-data-interface)
     (hresult rv interface)
   "Set an interface in the object's private data."   
   (name (& iid) :optional)
   (interface unknown))
  (private-data
      (hresult rv data-size)
    "Get a pointer to the object's data."
    (name (& guid))
    (data-size (& uint :inout))
    (data pointer))
  (parent
      (hresult rv (translate-interface
                    (com-interface-pointer unknown)
                    iid
                    t))
    "Gets the parent of the object."
    (iid (& iid))
    (unknown (& unknown :out) :aux)))

(define-interface device-sub-object
    ((iid-device-sub-object "{3d3e0379-f9de-4d58-bb6c-18d62992f1a6}") object)
  "Inherited from objects that are tied to the device so that they can retrieve a pointer to it."
  (get-device
      (hresult rv (translate-interface
                    (com-interface-pointer device)
                    iid
                    t))
    "Retrieves the device."
    (iid (& iid))
    (device (& unknown :out))))

(define-interface keyed-mutex
    ((iid-keyed-mutex "{9d8e1289-d7b3-465f-8126-250e349af85d}") device-sub-object)
  "Represents a keyed mutex, which allows exclusive access to a shared resource that is used by multiple devices."
  (acquire-sync
      (hresult)
    "Using a key, acquires exclusive rendering access to a shared resource."
    (key uint64)
    (milliseconds (enum (:base-type dword)
                    (:infinite #xFFFFFFFF))))
  (release-sync
      (hresult)
    "Using a key, releases exclusive rendering access to a shared resource."
    (key uint64)))

(define-interface resource
    ((iid-resource "{035f3ab4-482e-4e50-b41f-8a7f8bd8960b}") device-sub-object)
  "A dxgi:resource interface allows resource sharing and identifies the memory that a resource resides in."
  (shared-handle
      (hresult rv handle)
    "Get the handle to a shared resource."
    (handle (& handle :out) :aux))
  (usage
      (hresult rv usage)
    "Get the expected resource usage."
    (usage (& usage :out) :aux))
  ((setf eviction-priority)
     (hresult rv priority)
   "Set the priority for evicting the resource from memory."
   (priority resource-priority))
  (eviction-priority
      (hresult rv priority)
    "Get the eviction priority."
    (priority (& resource-priority :out) :aux)))

(define-interface surface
    ((iid-surface "{cafcb56c-6ac3-4889-bf47-9e23bbd260ec}") device-sub-object)
  "An dxgi:surface interface implements methods for image-data objects."
  (description
      (hresult rv desc)
    "Gets a description of an object."
    (desc (& surface-desc :out) :aux))
  (map-resource
      (hresult rv rect)
    "Gets a pointer to the data contained in the surface, and deny GPU access to the surface."
    (rect (& mapped-rect :out) :aux)
    (flags (enum (:base-type uint :list t)
             (:read 1)
             (:write 2)
             (:discard 4))))
  (unmap-resource
      (hresult)
    "Invalidate the pointer to the surface retrieved by dxgi:map-resource and re-enable GPU access to the resource."))

(define-interface surface*
    ((iid-surface* "{4AE63092-6327-4c1b-80AE-BFE12EA32B86}") surface)
  "The dxgi:surface* interface extends the dxgi:surface by adding support for rendering to a DXGI interface using GDI."
  (get-dc
      (hresult rv hdc)
    "Returns a device context (DC) that allows you to render to a DXGI surface using GDI."
    (discard boolean :optional)
    (hdc (& handle :out) :aux))
  (release-dc
      (hresult)
    "Releases the GDI device context (DC) associated with the current surface and allows rendering using Direct3D."
    (dirty-rect (& doors.gdi:rect :in t) :optional)))

(define-interface output
    ((iid-output "{ae02eedb-c735-4690-8d52-5a8dc20213aa}") object)
  "An dxgi:output interface represents an adapter output (such as a monitor)."
  (description
      (hresult rv desc)
    "Gets a description of the output."
    (desc (& output-desc :out) :aux))
  (display-mode-list
      (hresult rv (if (voidp modes)
                    num-modes
                    modes))
    "Gets the display modes that match the requested format and other input options."
    (enum-format dxgi-format)
    (flags enum-modes)
    (num-modes (& uint :inout) :optional 0)
    (modes (& (~ mode-desc nil list) :inout t)
           :aux (if (zerop num-modes)
                  void
                  (map-into (make-list num-modes) #'make-mode-desc))))
  (find-closest-matching-mode
      (hresult rv mode)
    "Finds the display mode that most closely matches the requested display mode."
    (mode-to-match (& mode-desc))
    (mode (& mode-desc :out) :aux)
    (concerned-device unknown :optional))
  (wait-for-vblank
      (hresult)
    "Halts a thread until the next vertical blank occurs.")
  (take-ownership
      (hresult)
    "Takes ownership of an output."
    (device unknown)
    (exclusive boolean))
  (release-ownership
      (void)
    "Releases ownership of the output.")
  (gamma-control-capabilities
      (hresult rv caps)
    "Gets a description of the gamma-control capabilities."
   (caps (& gamma-control-capabilities :out) :aux))
  ((setf gamma-control)
     (hresult rv array)
   "Sets the gamma controls."
   (array (& gamma-control)))
  (gamma-control
      (hresult rv array)
    "Gets the gamma control settings."
    (array (& gamma-control :out) :aux))
  ((setf display-surface)
      (hresult rv scanout-surface)
    "Changes the display mode."
    (scanout-surface surface))
  (display-surface-data
      (hresult rv destination)
    "Gets a copy of the current display surface."
    (destination surface))
  (frame-statistics
      (hresult rv stats)
    "Get statistics about recently rendered frames."
    (stats (& frame-statistics :out) :aux)))

(define-interface adapter
    ((iid-adapter "{2411e7e1-12ac-4ccf-bd14-9798e8534dc0}") object)
  "The dxgi:adapter interface represents a display sub-system (including one or more GPU's, DACs and video memory)."
  (enum-outputs
      (hresult rv output)
    "Enumerate adapter (video card) outputs."
    (index uint :optional)
    (output (& (output t) :out) :aux))
  (description
      (hresult rv desc)
    "Gets a DXGI 1.0 description of an adapter (or video card)."
    (desc (& adapter-desc :out) :aux))
  (check-interface-support
      (hresult rv version)
    "Checks to see if a device interface for a graphics component is supported by the system."
    (iid (& iid))
    (version (& int64 :out) :aux)))

(define-interface adapter*
    ((iid-adapter* "{29038f61-3839-4626-91fd-086879011a05}") adapter)
  "The dxgi:adapter* interface represents a display sub-system (including one or more GPU's, DACs and video memory)."
  (description*
      (hresult rv desc)
    "Gets a DXGI 1.1 description of an adapter (or video card)."
    (desc (& adapter-desc* :out) :aux)))

(define-interface swap-chain
    ((iid-swap-chain "{310d36a0-d2e7-4c0a-aa04-6a9d23b8886a}") device-sub-object)
  "A dxgi:swap-chain interface implements one or more surfaces for storing rendered data before presenting it to an output."
  (present
      (hresult)
    "Present a rendered image to the user."
    (sync-interval uint :optional)
    (flags present-flag :optional))
  (get-buffer
      (hresult rv (translate-interface
                    (com-interface-pointer surface)
                    iid
                    t))
    "Access one of the swap-chain back buffers."
    (buffer uint)
    (iid (& iid))
    (surface (& unknown :out) :aux))
  ((setf fullscreen-state)
     (hresult rv fullscreen)
   "Sets the display state to windowed or full-screen."
   (fullscreen boolean)
   (target output :optional))
  (fullscreen-state
      (hresult rv (values fullscreen target))
    "Get the state associated with full-screen mode."
    (fullscreen (& boolean :out) :aux)
    (target (& (output t) :out) :aux))
  (description
      (hresult rv desc)
    "Get a description of the swap chain."
    (desc (& swap-chain-desc :out) :aux))
  (resize-buffers
      (hresult rv)
    "Change the swap chain's back buffer size, format, and number of buffers. This should be called when the application window is resized."
    (buffer-count uint)
    (width uint :optional)
    (height uint :optional)
    (new-format dxgi-format :optional)
    (swap-chain-flags swap-chain-flag :optional))
  (resize-target
      (hresult rv)
    "Resize the output target."
    (new-target-parameters (& mode-desc)))
  (containing-output
      (hresult rv output)
    "Get the output (the display monitor) that contains the majority of the client area of the target window."
    (output (& (output t) :out) :aux))
  (frame-statistics
      (hresult rv stats)
    "Get performance statistics about the last render frame."
    (stats (& frame-statistics :out) :aux))
  (last-present-count
      (hresult rv count)
    "Get the number of times dxgi:present has been called."
    (count (& uint :out) :aux)))

(define-interface factory
    ((iid-factory "{7b7166ec-21c7-44ae-b21a-c9ae321ae369}") object)
  "An dxgi:factory interface implements methods for generating DXGI objects (which handle fullscreen transitions)."
  (enum-adapters
      (hresult rv adapter)
    "Enumerates the adapters (video cards)."
    (index uint :optional)
    (adapter (& (adapter t) :out) :aux))
  (make-window-association
      (hresult rv)
    "Allows DXGI to monitor an application's message queue for the alt-enter key sequence (which causes the application to switch from windowed to fullscreen or vice versa)."
    (window-handle handle)
    (flags (enum (:base-type uint :list t)
             (:no-window-changes #.(ash 1 0))
             (:no-alt-enter #.(ash 1 1))
             (:no-print-screen #.(ash 1 2))
             (:valid #x7))))
  (window-association
      (hresult rv handle)
    "Get the window through which the user controls the transition to and from fullscreen."
    (handle (& handle :out) :aux))
  (create-swap-chain
      (hresult rv swap-chain)
    "Creates a swap chain."
    (device unknown)
    (desc (& swap-chain-desc))
    (swap-chain (& (swap-chain t) :out) :aux))
  (create-software-adapter
      (hresult rv adapter)
    "Create an adapter interface that represents a software adapter."
    (module handle)
    (adapter (& (adapter t) :out) :aux)))

(define-interface factory*
    ((iid-factory* "{770aae78-f26f-4dba-a829-253c83d1b387}") factory)
  "The dxgi:factory* interface implements methods for generating DXGI objects."
  (enum-adapters*
      (hresult rv adapter)
    "Enumerates both adapters (video cards) with or without outputs."
    (index uint)
    (adapter (& (adapter t) :out) :aux))
  (is-current
      (boolean)
    "Informs an application of the possible need to re-enumerate adapters."))

(define-external-function
    ("CreateDXGIFactory" create-dxgi-factory)
    (:stdcall dxgi)
  (hresult rv factory)
  "Creates a DXGI 1.0 factory that generates objects used to enumerate and specify video graphics settings."
  (iid (& iid) :aux 'factory)
  (factory (& (factory t) :out) :aux))

(define-external-function
    ("CreateDXGIFactory1" create-dxgi-factory*)
    (:stdcall dxgi)
  (hresult rv factory)
  "Creates a DXGI 1.1 factory that generates objects used to enumerate and specify video graphics settings."
  (iid (& iid) :aux 'factory*)
  (factory (& (factory* t) :out) :aux))

(define-interface device
    ((iid-device "{54ec77fa-1377-44e6-8c32-88fd5f44c84c}") object)
  "A dxgi:device interface implements a derived class for DXGI objects that produce image data."
  (get-adapter
      (hresult rv adapter)
    "Returns the adapter for the specified device."
    (adapter (& (adapter t) :out) :aux))
  (create-surface
      (hresult rv surface)
    "Returns a surface. This method is used internally and you should not call it directly in your application."
    (desc (& surface-desc))
    (num-surfaces uint)
    (usage usage)
    (shared-resource (& shared-resource))
    (surface (& (~ (surface t) nil list) :out) :aux (make-list num-surfaces)))
  (resource-residency
      (hresult rv residency)
    "Gets the residency status of an array of resources."
    (resources (& (~ unknown)))
    (residency (& residency :out) :aux)
    (num-resources uint :optional (length resources)))
  ((setf gpu-thread-priority)
     (hresult rv priority)
   "Sets the GPU thread priority."
   (priority int))
  (gpu-thread-priority
      (hresult rv priority)
    "Gets the GPU thread priority."
    (priority (& int :out) :aux)))

(define-interface device*
    ((iid-device* "{77db970f-6276-48ba-ba28-070143b4392c}") device)
  "An dxgi:device* interface implements a derived class for DXGI objects that produce image data."
  ((setf maximum-frame-latency)
     (hresult rv max-latency)
   "Sets the number of frames that the system is allowed to queue for rendering."
   (max-latency uint))
  (maximum-frame-latency
      (hresult rv max-latency)
    "Gets the number of frames that the system is allowed to queue for rendering."
    (max-latency (& uint :out) :aux)))
