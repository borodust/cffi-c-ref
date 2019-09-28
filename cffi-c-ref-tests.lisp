(cl:defpackage :cffi-c-ref.tests
  (:use :cl :cffi-c-ref))
(5am:def-suite :cffi-c-ref.tests)

(cl:in-package :cffi-c-ref.tests)
(5am:in-suite :cffi-c-ref.tests)


(cffi:defcenum type
  :named
  :colored)


(cffi:defcenum color
  :red
  :black)


(cffi:defcstruct colored-object
  (color color))


(cffi:defcstruct named-object
  (name :char :count 16))


(cffi:defcstruct %object
  (type type)
  (data :pointer))


(cffi:defctype object (:struct %object))


(cffi:define-foreign-type %vec3 () ()
  (:actual-type :array :float 3)
  (:simple-parser %vec3))


(cffi:defctype indirect-vec3 %vec3)


(cffi:defctype direct-vec4 (:array :double 4))


(defun create-colored-object/cffi (object-color)
  (let ((object (cffi:foreign-alloc '(:struct %object)))
        (colored (cffi:foreign-alloc '(:struct colored-object))))
    (cffi:with-foreign-slots ((type data) object (:struct %object))
      (setf type :colored
            data colored))
    (cffi:with-foreign-slots ((color) colored (:struct colored-object))
      (setf color object-color))
    object))


(defun create-colored-object/c-ref (object-color)
  (c-let ((object (:struct %object) :alloc t)
          (colored (:struct colored-object) :alloc t))
    (setf (object :type) :colored
          (object :data) (colored &)
          (colored :color) object-color)
    (object &)))


(defun create-named-object/cffi (object-name)
  (let ((object (cffi:foreign-alloc '(:struct %object)))
        (named (cffi:foreign-alloc '(:struct named-object))))
    (cffi:with-foreign-slots ((type data) object (:struct %object))
      (setf type :named
            data named)
      (cffi:with-foreign-slots ((name) named (:struct named-object))
        (loop for char across object-name
              for i from 0 below 15
              do (setf (cffi:mem-ref name :char i) (char-code char))
              finally (setf (cffi:mem-ref name :char (1+ i)) 0)))
      object)))


(defun create-named-object/c-ref (object-name)
  (c-let ((object (:struct %object) :alloc t)
          (named (:struct named-object) :alloc t))
    (setf (object :type) :named
          (object :data) (named &))
    (loop for char across object-name
          for i from 0 below 15
          do (setf (named :name i) (char-code char))
          finally (setf (named :name (1+ i)) 0))
    (object &)))


(defun get-object-detail/cffi (object)
  (cffi:with-foreign-slots ((type data) object object)
    (ecase type
      (:named
       (cffi:with-foreign-slots ((name) data (:struct named-object))
         (cffi:foreign-string-to-lisp name :max-chars 16)))
      (:colored
       (cffi:with-foreign-slots ((color) data (:struct colored-object))
         color)))))


(defun get-object-detail/c-ref (object)
  (c-val ((object object))
    (ecase (object :type)
      (:named
       (c-let ((named (:struct named-object) :from (object :data)))
         (cffi:foreign-string-to-lisp (named :name &) :max-chars 16)))
      (:colored (object (:data (:pointer (:struct colored-object))) * :color)))))


(defun destroy-object/cffi (object)
  (cffi:with-foreign-slots ((data) object object)
    (cffi:foreign-free data)
    (cffi:foreign-free object)))


(defun destroy-object/c-ref (object)
  (c-val ((object (:struct %object)))
    (cffi:foreign-free (object :data))
    (cffi:foreign-free (object &))))


(5am:test named-object-indirect-fill
  (c-with ((obj (:struct %object))
           (named (:struct named-object)))
    (setf (obj :data) (named &)
          (obj :type) :named)
    (loop for char across "test"
          for i from 0
          do (setf
              (obj (:data (:pointer (:struct named-object))) * :name i)
              (char-code char))
          finally (setf (named :name (1+ i)) 0))
    (5am:is (equal "test" (cffi:foreign-string-to-lisp (named :name &)
                                                       :max-chars 16)))))

(5am:test object-handling
  (let ((named/c-ref (create-named-object/c-ref "HI"))
        (named/cffi (create-named-object/cffi "HI"))
        (colored/c-ref (create-colored-object/c-ref :black))
        (colored/cffi (create-colored-object/cffi :black)))
    (unwind-protect
         (progn
           (5am:is (equal :black (get-object-detail/c-ref colored/c-ref)))
           (5am:is (equal "HI" (get-object-detail/c-ref named/c-ref)))

           (5am:is (equal :black (get-object-detail/cffi colored/c-ref)))
           (5am:is (equal "HI" (get-object-detail/cffi named/c-ref)))

           (5am:is (equal :black (get-object-detail/c-ref colored/cffi)))
           (5am:is (equal "HI" (get-object-detail/c-ref named/cffi))))
      (destroy-object/cffi named/c-ref)
      (destroy-object/c-ref colored/cffi)
      (destroy-object/cffi named/cffi)
      (destroy-object/c-ref colored/c-ref))))


(5am:test simple-dereference
  (c-with ((val (:pointer :int) :count 10 :clear t))
    (loop for i from 0 below 10
          do (setf (val i) (cffi:foreign-alloc :int)
                   (val i *) i))
    (5am:is (equal
             (loop for i from 0 below 10
                   collect i)
             (loop for i from 0 below 10
                   collect (val i *)
                   do (cffi:foreign-free (val i)))))))


(5am:test direct-array-dereference
  (c-with ((val direct-vec4))
    (setf (val * 0) 0d0
          (val * 1) 1d0
          (val * 2) 2d0)
    (5am:is (equal (list 0d0 1d0 2d0)
                   (list (val * 0) (val * 1) (val * 2))))))


(5am:test indirect-array-dereference
  (c-with ((val indirect-vec3))
    (setf (val * 0) 0f0
          (val * 1) 1f0
          (val * 2) 2f0)
    (5am:is (equal (list 0f0 1f0 2f0)
                   (list (val * 0) (val * 1) (val * 2))))))
