(asdf:defsystem :cffi-c-ref
  :description "Adds streamlined access to foreign memory"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :version "1.0"
  :depends-on (:alexandria :cffi)
  :components ((:file "cffi-c-ref")))


(asdf:defsystem :cffi-c-ref/tests
  :description "Tests for :cffi-c-ref"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :version "1.0"
  :depends-on (:alexandria :cffi-c-ref :fiveam :bodge-libc-essentials)
  :components ((:file "cffi-c-ref-tests")))
