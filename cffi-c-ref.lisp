(cl:defpackage :cffi-c-ref
  (:use :cl :alexandria)
  (:export #:c-ref
           #:c-let
           #:c-with
           #:c-val))
(cl:in-package :cffi-c-ref)


(defun %mem-offset (ptr type offset dynamic-offset accessors)
  (labels ((find-slot-name (keyword-name)
             (let ((slot-names (cffi:foreign-slot-names type)))
               (if-let ((slot (find (symbol-name keyword-name)
                                    slot-names
                                    :key #'symbol-name
                                    :test #'string=)))
                 slot
                 (error "Slot with name ~A not found. Available names: ~{~A~^, ~}"
                        keyword-name (mapcar #'symbol-name slot-names)))))
           (%mem-offset-slot (accessor &optional next-type)
             (let* ((slot-name (find-slot-name accessor))
                    (next-type (or next-type (cffi:foreign-slot-type type slot-name)))
                    (next-offset (cffi:foreign-slot-offset type slot-name)))
               (%mem-offset ptr next-type
                            (+ offset next-offset)
                            dynamic-offset
                            (rest accessors))))
           (%mem-offset-array (accessor)
             (%mem-offset ptr type
                          (+ offset (* (cffi:foreign-type-size type)
                                       accessor))
                          dynamic-offset
                          (rest accessors)))
           (%mem-offset-with-cast (accessor)
             (destructuring-bind (slot-name slot-type)
                 accessor
               (%mem-offset-slot slot-name slot-type)))
           (%mem-offset-dynamically (accessor)
             (%mem-offset ptr type
                          offset
                          (list* (if (> (cffi:foreign-type-size type) 1)
                                     `(* ,(cffi:foreign-type-size type) ,accessor)
                                     accessor)
                                 dynamic-offset)
                          (rest accessors)))
           (%expand-offset ()
             (cond
               ((and (zerop offset) (null dynamic-offset))
                0)
               ((and (not (zerop offset)) dynamic-offset)
                `(+ ,offset ,@dynamic-offset))
               ((and (zerop offset) (rest dynamic-offset))
                `(+ ,@dynamic-offset))
               ((zerop offset) (first dynamic-offset))
               (t offset)))
           (%mem-offset-symbol (accessor)
             (switch (accessor :test #'string=)
               ("&" (when (rest accessors)
                      (error "& must be the last accessor, but ~A more found"
                             (rest accessors)))
                    (if (and (null dynamic-offset) (zerop offset))
                        ptr
                        `(cffi:inc-pointer ,ptr ,(%expand-offset))))
               ("*" (destructuring-bind (kind &optional ptr-type &rest rest)
                        (ensure-list type)
                      (declare (ignore rest))
                      (unless (eq kind :pointer)
                        (error "Cannot dereference a non-pointer ~A" type))
                      (when (or (not ptr-type) (eq :void ptr-type))
                        (error "Cannot dereference a void pointer"))
                      (%mem-offset `(cffi:mem-ref ,ptr ',type ,(%expand-offset)) ptr-type
                                   0 nil
                                   (rest accessors))))
               (t (%mem-offset-dynamically accessor)))))
    (if accessors
        (let ((accessor (first accessors)))
          (etypecase accessor
            (symbol (if (keywordp accessor)
                        (%mem-offset-slot accessor)
                        (%mem-offset-symbol accessor)))
            (integer (%mem-offset-array accessor))
            (cons (if (keywordp (first accessor))
                      (%mem-offset-with-cast accessor)
                      (%mem-offset-dynamically accessor)))))
        `(cffi:mem-ref ,ptr ',type ,(%expand-offset)))))


(defmacro c-ref (ptr type &rest accessors)
  (%mem-offset ptr type 0 nil accessors))


(cffi:defcfun (%memset "memset") :void
  (destination (:pointer :void))
  (initial-value :int)
  (size :unsigned-int))


(defmacro c-let ((&rest bindings) &body body)
  (with-gensyms (accessors)
    (multiple-value-bind (macrolets
                          symbol-macrolets
                          allocators
                          deallocators
                          initializers
                          dynamic)
        (loop with macrolets
              with symbol-macrolets
              with allocators
              with deallocators
              with initializers
              with dynamic
              for binding in bindings
              do (destructuring-bind (var type &key from alloc free (count 1) clear)
                     binding
                   (when (and alloc from)
                     (error ":alloc and :from both found in ~A" binding))
                   (unless (or alloc from)
                     (error "Neither :alloc nor :from found in ~A" binding))
                   (with-gensyms (ptr)
                     (if (and alloc free)
                         (push `(,ptr ,type :count ,count)
                               dynamic)
                         (progn
                           (when alloc
                             (push `(,ptr (cffi:foreign-alloc ',type :count ,count))
                                   allocators))
                           (when free
                             (push `(cffi:foreign-free ,ptr) deallocators))))
                     (when from
                       (push `(,ptr ,from) allocators))
                     (when clear
                       (push `(%memset ,ptr 0 ,(* (cffi:foreign-type-size type)
                                                  count))
                             initializers))
                     (push `(,var (&rest ,accessors)
                                  `(c-ref ,',ptr ,',type ,@,accessors))
                           macrolets)
                     (push `(,var (c-ref ,ptr ,type))
                           symbol-macrolets)))
              finally (return (values macrolets
                                      symbol-macrolets
                                      allocators
                                      deallocators
                                      initializers
                                      dynamic)))
      `(let ,allocators
         (cffi:with-foreign-objects ,dynamic
           (macrolet ,macrolets
             (symbol-macrolet ,symbol-macrolets
               (unwind-protect
                    (progn
                      ,@initializers
                      ,@body)
                 ,@deallocators))))))))


(defmacro c-with ((&rest bindings) &body body)
  `(c-let ,(loop for (var type . rest) in bindings
                 collect `(,var ,type :alloc t :free t ,@rest))
     ,@body))


(defmacro c-val ((&rest bindings) &body body)
  `(c-let ,(loop for (var type) in bindings
                 collect `(,var ,type :from ,var))
     ,@body))
