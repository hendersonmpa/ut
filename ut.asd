;;;; ut.asd

(asdf:defsystem #:ut
  :description "Describe lab here"
  :author "Matthew Henderson"
  :license "Specify license here"
  :depends-on (:parse-number
               :local-time
               :split-sequence
               :alexandria
               :cl-store
               :cl-ppcre
               :group-by
;;               :clsql
               :cl-dbi
               :lhstats
               :hunchentoot
               :parenscript
               :cl-who
               :eazy-gnuplot
               :open-geneva)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "queries")
               (:file "dataset")
               (:file "analyte")
               (:file "group")
               (:file "plots")
               (:file "division")
               (:file "report")
               (:file "server")
               (:file "pages")))
