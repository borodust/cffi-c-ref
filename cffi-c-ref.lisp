(cl:defpackage :cffi-c-ref
  (:use :cl :alexandria)
  (:export #:c-ref
           #:c-let
           #:c-with
           #:c-val))
(cl:in-package :cffi-c-ref)


(defun %mem-offset (ptr offset type accessors)
  (if accessors
      (let ((accessor (first accessors)))
        (etypecase accessor
          (symbol
           (switch (accessor :test #'string=)
             ("&" (when (rest accessors)
                    (error "& must be the last accessor, but ~A more found" accessors))
                  `(cffi:incf-pointer ,ptr ,offset))
             ("*" (destructuring-bind (kind &optional ptr-type &rest rest)
                      (ensure-list type)
                    (declare (ignore rest))
                    (unless (eq kind :pointer)
                      (error "Cannot dereference a non-pointer ~A" type))
                    (unless ptr-type
                      (error "Cannot dereference a void pointer"))
                    (%mem-offset `(cffi:inc-pointer ,ptr ,offset)
                                 0 ptr-type (rest accessors))))
             (t (let ((next-type (cffi:foreign-slot-type type accessor))
                      (next-offset (cffi:foreign-slot-offset type accessor)))
                  (%mem-offset ptr (+ offset next-offset) next-type (rest accessors))))))
          (integer (%mem-offset ptr (+ offset (* (cffi:foreign-type-size type) accessor))
                                type
                                (rest accessors)))))
      `(cffi:mem-ref ,ptr ',type ,offset)))


(defmacro c-ref (ptr type &rest accessors)
  (%mem-offset ptr 0 type accessors))


(defmacro c-let ((&rest bindings) &body body)
  (with-gensyms (accessors)
    (multiple-value-bind (macrolets symbol-macrolets allocators deallocators)
        (loop with macrolets
              with symbol-macrolets
              with allocators
              with deallocators
              for binding in bindings
              do (destructuring-bind (var type &key from alloc free (count 1))
                     binding
                   (when (and alloc from)
                     (error ":alloc and :from both found in ~A" binding))
                   (with-gensyms (ptr)
                     (when alloc
                       (push `(,ptr (cffi:foreign-alloc ',type :count ,count))
                             allocators))
                     (when from
                       (push `(,ptr ,from) allocators))
                     (when free
                       (push `(cffi:foreign-free ,ptr) deallocators))
                     (push `(,var (&rest ,accessors)
                                  `(c-ref ,',ptr ,',type ,@,accessors))
                           macrolets)
                     (push `(,var (c-ref ,ptr ,type))
                           symbol-macrolets)))
              finally (return (values macrolets
                                      symbol-macrolets
                                      allocators
                                      deallocators)))
      `(let ,allocators
         (macrolet ,macrolets
           (symbol-macrolet ,symbol-macrolets
             (unwind-protect
                  (progn ,@body)
               ,@deallocators)))))))


(defmacro c-with ((&rest bindings) &body body)
  `(c-let ,(loop for (var type) in bindings
                 collect `(,var ,type :alloc t :free t))
     ,@body))


(defmacro c-val ((&rest bindings) &body body)
  `(c-let ,(loop for (var type) in bindings
                 collect `(,var ,type :from ,var))
     ,@body))
