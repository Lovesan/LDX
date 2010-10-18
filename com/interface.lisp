(in-package #:ldx.com)

(defvar *pointer-to-object-mapping* (make-weak-hash-table :test #'eql
                                      :weakness :value))

(defvar *com-interface-types* (make-hash-table :test #'eq))

(defvar *pointer-to-interface-mapping* (make-weak-hash-table :test #'eql
                                         :weakness :value))

(declaim (inline com-interface com-interface-pointer))
(defstruct (com-interface
             (:constructor nil)
             (:conc-name nil))
  (com-interface-pointer &0 :type pointer))

(define-immediate-type com-interface-type ()
  ((name :initform nil
         :initarg :name
         :reader com-interface-type-name)
   (parent :initform nil
           :initarg :parent
           :reader com-interface-type-parent)
   (children :initform '()
             :initarg :children
             :accessor com-interface-type-children)
   (vtable-name :initform nil
                :initarg :vtable-name
                :reader com-interface-type-vtable-name)
   (constructor-name :initarg :constructor-name
                     :reader com-interface-type-constructor-name
                     :initform 'com-interface)
   (methods :initform '()
            :initarg :methods
            :reader com-interface-type-methods))
  (:base-type pointer)
  (:lisp-type (type) (com-interface-type-name type))
  (:converter (lisp-value type)
    (if (null lisp-value)
      &0
      (com-interface-pointer lisp-value)))
  (:translator (pointer type)
    (if (&? pointer)
      (let* ((address (&& pointer))
             (interface (gethash address *pointer-to-interface-mapping*)))
        (if (and interface (typep interface (com-interface-type-name type)))
          interface
          (setf (gethash address *pointer-to-interface-mapping*)
                (funcall (com-interface-type-constructor-name type) pointer))))
      nil))
  (:converter-expansion (lisp-value-form type)
    (with-gensyms (interface)
      `(let ((,interface ,lisp-value-form))
         (if (null ,interface)
           &0
           (com-interface-pointer ,interface)))))
  (:translator-expansion (pointer-form type)
    (with-gensyms (pointer address interface)
      `(let* ((,pointer ,pointer-form)
              (,address (&& ,pointer))
              (,interface (gethash ,address *pointer-to-interface-mapping*)))
         (declare (type pointer ,pointer)
                  (type size-t ,address))
         (if (and ,interface (typep ,interface ',(com-interface-type-name type)))
           ,interface
           (setf (gethash ,address *pointer-to-interface-mapping*)
                 (,(com-interface-type-constructor-name type) ,pointer))))))
  (:allocator-expansion (value type)
    `(alloc 'pointer))
  (:deallocator-expansion (pointer type)
    `(free ,pointer '*))
  (:cleaner-expansion (pointer value type)
    nil))

(defmethod unparse-type ((type com-interface-type))
  (com-interface-type-name type))

(defun normalize-arg-spec (arg-spec)
  (destructuring-bind
      (name typespec &optional (kind :primary) (initform nil initform-p))
      arg-spec
    (list arg-spec typespec name kind initform initform-p)))

(defun parse-args (args)
  (loop :with names = '()
    :for (original typespec name kind initform initform-p)
    :in (mapcar #'normalize-arg-spec args)
    :do (cond
          ((member name names)
           (error "Duplicate argument names exist: ~s" args))
          ((not (member kind '(:primary :key :aux :optional)))
           (error "Invalid argument kind: ~s in ~s"
                  kind original))
          ((and (eq kind :primary) initform-p)
           (error "Initforms for :primary arguments are not allowed ~s"
                  original))
          (T (push name names)))
    :if (eq kind :primary) :collect name :into primary
    :if (eq kind :key) :collect name :into key
    :if (eq kind :optional) :collect name :into optional
    :if (eq kind :aux) :collect name :into aux
    :collect (list typespec name :aux name) :into normalized
    :collect (let ((type (parse-typespec typespec)))
               (list name
                     (lisp-type type)
                     (if (or initform-p (eq kind :primary))
                       initform
                       (expand-prototype type))))
    :into types
    :finally (if (and key optional)
               (error "~s and ~s arguments in the same argument list are not allowed ~s"
                      :key :optional args)
               (return (values normalized
                               types
                               primary
                               key
                               optional
                               aux)))))

(defun make-method-arg-group (types group group-name)
  `(,group-name ,@(mapcar (lambda (name)
                            (list name (third (assoc name types))))
                    group)))
  
(defun parse-method-spec (interface-name vtable-name method-name vtable-index method-spec)
  (destructuring-bind
      ((return-type &optional (return-value-name (gensym))
                    (result-form return-value-name))
       &rest doc-and-args)
      method-spec
    (check-type method-name symbol)
    (check-type return-value-name symbol)
    (let ((doc (if (stringp (car doc-and-args))
                 (car doc-and-args)
                 nil))
          (args (if (stringp (car doc-and-args))
                  (rest doc-and-args)
                  doc-and-args))
          (this-var (intern (string 'this)))
          (rv-var (gensym (string 'return-value))))
      (multiple-value-bind
          (normalized types primary key optional aux) (parse-args args)
        `((defmethod ,method-name (,@(if (consp method-name)
                                       `(,(car primary)
                                           (,this-var ,interface-name)
                                           ,@(rest primary))
                                       `((,this-var ,interface-name)
                                         ,@primary))
                                      ,@(unless (null key)
                                          (make-method-arg-group types key '&key))
                                      ,@(unless (null optional)
                                          (make-method-arg-group types optional '&optional))
                                      ,@(unless (null aux)
                                          (make-method-arg-group types aux '&aux)))
            (declare (type ,interface-name ,this-var)
                     ,@(loop :for (name lisp-type initform) :in types
                        :collect `(type ,lisp-type ,name)))
            ,@(ensure-list doc)
            (external-pointer-call
              (deref (&+ (deref (com-interface-pointer ,this-var) 'pointer)
                      ,vtable-index
                      'pointer)
                     'pointer)
              ((:stdcall)
               (,return-type ,return-value-name ,result-form)
               (,interface-name ,this-var :aux ,this-var)
               ,@normalized)))
          ,@(let ((trampoline-name (intern (format nil "~a::~a::~a-~a"
                                                   (package-name *package*)
                                                   interface-name
                                                   method-name
                                                   'trampoline)
                                           :ldx.com)))
             `((progn (define-callback (,trampoline-name :stdcall) ,return-type
                        ((,this-var size-t)
                         ,@(loop :for (arg-name arg-typespec . rest) :in args :collect
                             (list arg-name arg-typespec)))
                        (let ((,rv-var ,(expand-prototype (parse-typespec return-type))))
                          (multiple-value-setq
                            (,rv-var ,@(mapcar #'car args))
                            (,method-name ,@(if (consp method-name)
                                              `(,(car primary)
                                                  (gethash ,this-var *pointer-to-object-mapping*)
                                                  ,@(rest primary))
                                              `((gethash ,this-var *pointer-to-object-mapping*)
                                                ,@primary))
                                          ,@optional
                                          ,@(loop :for name :in key :nconc
                                              (list (intern (string name) :keyword) name))))
                          ,rv-var))
                      (setf (deref ,vtable-name 'pointer ,(* vtable-index (sizeof '*)))
                            (callback ,trampoline-name)))
               ,trampoline-name)))))))

(defmacro define-interface (name-and-options
                             (&optional iid-name (parent-name nil parent-name-p))
                             &rest doc-and-methods)
  (check-type parent-name symbol)
  (check-type name-and-options (or symbol cons))
  (check-type iid-name symbol)
  (destructuring-bind
      (name &key (constructor-name name)
            (vtable-name (intern (format nil "~a::~a-~a"
                                         (package-name *package*) name 'vtable)
                                 :ldx.com)))
      (ensure-list name-and-options)
    (let* ((doc (if (stringp (car doc-and-methods))
                  (car doc-and-methods)
                  nil))
           (methods (if (stringp (car doc-and-methods))
                      (rest doc-and-methods)
                      doc-and-methods))
           (parent (if parent-name-p
                     (or (gethash parent-name *com-interface-types*)
                         (error "Interface named ~s is undefined" parent-name))
                     nil))
           (parent-methods (if parent-name-p
                             (loop :for (method-name overloaded-p)
                               :in (if parent (com-interface-type-methods parent) '())
                               :if (assoc method-name methods :test #'equal)
                               :collect (list method-name t)
                               :else :collect (list method-name nil))))
           (direct-methods (loop :for (method-name . method-spec) :in methods
                             :unless (assoc method-name parent-methods :test #'equal)
                             :collect (list method-name nil)))
           (parent-vtable (when parent-name-p (com-interface-type-vtable-name parent)))
           (vtable-size (+ (length parent-methods) (length direct-methods))))
      (check-type name symbol)
      (check-type constructor-name symbol)
      (check-type vtable-name symbol)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (declaim (inline ,constructor-name))
           (defstruct (,name
                        (:include ,(if parent-name-p
                                     parent-name
                                     'com-interface))
                        (:constructor ,constructor-name (com-interface-pointer)))
             ,@(ensure-list doc))
           (define-type-parser ,name ()
             (gethash ',name *com-interface-types*)))
         (defvar ,vtable-name (alloc '(simple-array uint-ptr (,vtable-size))))
         ,@(when parent-name-p
            `((setf (deref ,vtable-name
                           '(simple-array uint-ptr (,(length parent-methods))))
                    (deref ,parent-vtable
                           '(simple-array uint-ptr (,(length parent-methods)))))))
       ,@(when iid-name
           `((register-uuid ,iid-name ,name)))
       ,@(loop :with vtable-index = (length parent-methods)
             :for (method-name . method-spec) :in methods
             :for (method-def trampoline-def trampoline-name) =
             (parse-method-spec
               name                              
               vtable-name
               method-name
               (or (position method-name parent-methods :key #'car
                             :test #'equal)
                   (prog1 vtable-index (incf vtable-index)))
               method-spec)
             :collect method-def :into method-defs
             :collect trampoline-def :into trampoline-defs
             :finally (return `((eval-when (:compile-toplevel :load-toplevel :execute)
                                  (let (,@(when parent-name-p
                                            `((parent (gethash ',parent-name
                                                               *com-interface-types*)))))
                                    (setf (gethash ',name *com-interface-types*)
                                          (make-instance 'com-interface-type
                                            :name ',name
                                            :parent ,(when parent-name-p 'parent)
                                            :vtable-name ',vtable-name
                                            :constructor-name ',constructor-name
                                            :methods '(,@parent-methods ,@direct-methods)))
                                    ,(when parent-name-p
                                       `(pushnew (gethash ',name *com-interface-types*)
                                                 (com-interface-type-children parent)))))
                                ,@method-defs
                                ,@trampoline-defs)))
       ',name))))

(defmacro define-interface-method
    (interface-name method-name
     (return-type &optional (return-value-name (gensym))
                            (result-form return-value-name))
     &body doc-and-args)
  (check-type interface-name symbol)
  (check-type method-name symbol)
  (check-type return-value-name symbol)
  (let* ((type (or (gethash interface-name *com-interface-types*)
                   (error "Interface named ~s is undefined"
                          interface-name)))
         (vtable-index (or (position method-name (com-interface-type-methods type)
                                     :key #'car :test #'equal)
                           (error "Interface ~s has no method named ~s"
                                  interface-name method-name))))
    (destructuring-bind
        (method-def trampoline-def trampoline-name)
        (parse-method-spec interface-name
                           (com-interface-type-vtable-name type)
                           method-name
                           vtable-index
                           `((,return-type ,return-value-name ,result-form)
                             ,@doc-and-args))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (second (assoc ',method-name
                                (com-interface-type-methods
                                  (gethash ',interface-name *com-interface-types*))
                                :test #'equal))
                 t))
         ,method-def
         ,trampoline-def
         ,(labels ((frob (children)
                     (loop :for type :in children
                       :for child-vtable = (com-interface-type-vtable-name type)
                       :for child-methods = (com-interface-type-methods type)
                       :unless (second (assoc method-name child-methods :test #'equal))
                       :append (cons
                                 `(setf (deref ,child-vtable 'pointer ,(* vtable-index (sizeof '*)))
                                        (callback ,trampoline-name))
                                 (ensure-list (frob (com-interface-type-children type)))))))
            `(progn ,@(frob (com-interface-type-children type))))
         ',method-name))))
