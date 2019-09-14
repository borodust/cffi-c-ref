(asdf:defsystem :cffi-c-ref
  :description "Adds streamlined access to foreign memory"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :version "1.0"
  :depends-on (:alexandria :cffi)
  :components ((:file "cffi-c-ref")))
