(defsystem :cl-travis-test
  :depends-on (:rt :dummy-cl-travis-system)
  :components ((:file "cl-travis-test")))
