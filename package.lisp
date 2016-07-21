;;;; package.lisp

(defpackage :ut
  (:use :cl
        :parenscript
        :cl-who
        :hunchentoot))

(rename-package "CL-PPCRE" "CL-PPCRE" '("RE"))
(rename-package "HUNCHENTOOT" "HUNCHENTOOT" '("HUNCH"))
(rename-package "CL-WHO" "CL-WHO" '("WHO"))
(rename-package "STATISTICS" "STATISTICS" '("STATS"))
(rename-package "GENEVA" "GENEVA" '("GEN"))

(in-package :ut)
(defparameter *figures-dir* "/home/mpah/lisp/site/ut/figures/"
  "Used throughout to store files and figures")
