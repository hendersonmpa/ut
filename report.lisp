;;; file report.lisp
;;; Functions to create a pdf report
;;; http://inters.co/geneva/open-geneva.html
(in-package :ut)

;;(geneva:in-readtable geneva.macros:syntax)

(defun make-overview (division-name location-list)
  (let ((directory-string
         (concatenate 'string *figures-dir* division-name "/")))
    (flet ((make-url (file-name)
             (concatenate 'string directory-string division-name file-name)))
      (gen:make-section
       `(,(format nil "Overview of test utilization in ~:(~a~)" division-name))
       (list
        (gen:make-paragraph
         `("This report summarizes one year of laboratory testing ordered in the following locations:"))
        (gen:make-listing
         (mapcar #'list location-list))
        (gen:make-section
         `(,(format nil "Overview of the top 20 most frequently ordered tests in ~:(~a~) " division-name))
         (list (gen:make-media
                `(,(format nil "Bar chart of the top 20 tests in ~:(~a~)" division-name))
                (make-url "barcharttest.png"))
               (gen:make-media
                `(,(format nil "Count vs unique mrns for each test in ~:(~a~)" division-name))
                (make-url "xytestname.png"))))
        (gen:make-section
         `(,(format nil "Overview laboratory test utilization by providers in ~:(~a~)"
                    division-name))
         (list (gen:make-media
                `(,(format nil "Bar chart of the top 20 providers in ~:(~a~)" division-name))
                (make-url "barchartprovider.png"))
               (gen:make-media
                `(,(format nil "Count vs unique mrns for each provider in ~:(~a~)" division-name))
                (make-url "xyprovider.png"))))
        (gen:make-section
         `(,(format nil "Overview of provider specialties in ~:(~a~)" division-name))
         (list (gen:make-media
                `(,(format nil "Bar chart of the specialties in ~:(~a~)" division-name))
                (make-url "barchartspecialty.png"))
               (gen:make-media
                `(,(format nil "Count vs unique mrns for each specialty in ~:(~a~)" division-name))
                (make-url "xyspecialty.png")))))))))

(defun make-analyte-section (division-name test-name)
  (let ((directory-string
         (concatenate 'string *figures-dir* division-name "/")))
    (flet ((make-url (file-name)
             (concatenate 'string directory-string test-name file-name)))
      (gen:make-section
       `(,(format nil "~:@(~a~) utilization in ~:(~a~)" test-name division-name))
       (list
        (gen:make-section
         `(,(format nil "~:@(~a~) orders by provider in ~:(~a~)" test-name division-name))
         (list (gen:make-media
                `(,(format nil "Bar chart ~:@(~a~) use in ~:(~a~)" test-name division-name))
                (make-url "barchartprovider.png"))
               (gen:make-media
                `(,(format nil "~:@(~a~) test count vs unique mrns for each provider in ~:(~a~)" test-name division-name))
                (make-url "xyprovider.png"))))
        (gen:make-section
         `(,(format nil "Time series analysis of ~:@(~a~) use in ~:(~a~)" test-name division-name))

         (list (gen:make-media
                `(,(format nil "Time series of ~:@(~a~) testing in top 20 patients in ~:(~a~)" test-name division-name))
                (make-url "timeseries.png"))
               (gen:make-paragraph
                `(,(format nil "Overview of all laboratory testing preformed in the one ~:(~a~) patient" division-name)))
               (gen:make-media
                `("Test series")
                (make-url "testseries.png")))))))))

(defun make-report (division-name location-list test-list)
  (let ((accum (list (make-overview division-name location-list))))
    (gen:make-document
     (dolist (test test-list (reverse accum))
       (push (make-analyte-section division-name test) accum)))))

(defun make-pdf (division-name location-list test-list)
  (with-open-file (out
                   (merge-pathnames *figures-dir* "report.tex") :direction :output
                   :if-exists :supersede)
    (geneva.latex:render-latex (make-report division-name location-list test-list)
                               :stream out
                               :title (format nil "Division of ~:(~a~) Utilization Report" division-name)
                               :author "Matthew P.A. Henderson")))


;;; (make-pdf "peds" '("4 West" "4 east" "5 east") '("creatinine" "tsh"))
(make-pdf "peds" '("4 West" "4 east" "5 east") '("creatinine" "tsh"))
