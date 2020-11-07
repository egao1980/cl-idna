(defpackage cl-idna/tests/main
  (:use :cl
        :cl-idna
        :rove))
(in-package :cl-idna/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-idna)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
