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


(cffi:defcstruct object
  (type type)
  (data :pointer))


(defun create-colored-object (object-color)
  (let ((object (cffi:foreign-alloc '(:struct object)))
        (colored (cffi:foreign-alloc '(:struct colored-object))))
    (cffi:with-foreign-slots ((type data) object (:struct object))
      (setf type :colored
            data colored))
    (cffi:with-foreign-slots ((color) colored (:struct colored-object))
      (setf color object-color))
    object))


(defun create-named-object (object-name)
  (c-let ((object (:struct object) :alloc t)
          (named (:struct named-object) :alloc t))
    (setf (object :type) :named
          (object :data) (named &))
    (loop for char across object-name
          for i from 0 below 15
          do (setf (named :name i) (char-code char))
          finally (setf (named :name (1+ i)) 0))
    (object &)))


(defun get-object-detail (object)
  (c-val ((object (:struct object)))
    (ecase (object :type)
      (:named
       (c-let ((named (:struct named-object) :from (object :data)))
         (cffi:foreign-string-to-lisp (named :name &) :max-chars 16)))
      (:colored (object (:data (:pointer (:struct colored-object))) * :color)))))


(defun destroy-object (object)
  (c-val ((object (:struct object)))
    (cffi:foreign-free (object :data))
    (cffi:foreign-free (object &))))


(5am:test named-object-indirect-fill
  (c-with ((obj (:struct object))
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
  (let ((named (create-named-object "HI"))
        (colored (create-colored-object :black)))
    (unwind-protect
         (progn
           (5am:is (equal :black (get-object-detail colored)))
           (5am:is (equal "HI" (get-object-detail named))))
      (destroy-object named)
      (destroy-object colored))))


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
