(in-package :cl-user)

(ql:quickload 'cl-ppcre)

(defpackage :com.dsmith.fst
  (:use :common-lisp)
  (:import-from :cl-ppcre :split))
