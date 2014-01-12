(defpackage :cl-travis-test
  (:use :cl :rtest))

(in-package :cl-travis-test)

(deftest simple-test
    (not (member :force-test-failure *features*))
  t)
