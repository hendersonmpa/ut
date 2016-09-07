;;;; file plots.lisp

(in-package #:ut)

;(setf cl-ana.plotting::*gnuplot-file-io* 'nil)

(defun histogram (dataset)
  "Plot a histogram of the data set"
  (let* ((file-name (merge-pathnames *figures-dir* "histogram.png"))
         (data (data dataset))
         (component-name (getf (car data) :component-name))
         (results (remove nil
                          (collect-column data
                                          :col :clean-result)))
         ;; TODO: remove outliers
         (h (fd-bins results)) ;; bin-width
         (units (units dataset)))
    (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (format t "binwidth = ~S~%" h)
      (format t "binstart = ~S~%" (* -2 h))
      (eazy-gnuplot:gp-setup :output file-name :terminal '(png :size (1200 742) enhanced font "Verdana,8")
                             :key '(off)
                             :title component-name
                             :ylabel "count"
                             :xlabel units
                             :ytic '(nomirror font ",8")
                             :xtic '(nomirror font ",8")
                             :tic '(nomirror out scale 0.75)
                             :border '(3 front linecolor rgb ("'#808080'") lt 1)
                             :boxwidth '(0.9*binwidth)
                             :style '(fill solid 0.5 noborder)
                             :style '(line 1 linetype 1 lc rgb ("'#71637D'") lw 1)
                             :style '(line 2 linetype 3 lc rgb ("'#3288BD'") lw 1 pointtype 4)
                             :xrange '("[0:]")
                             :yrange '("[-50:]")
                             :xzeroaxis '())
      (eazy-gnuplot:plot
       (lambda ()
         (format t "~&~{~a ~%~}" results))
       :using '("1:(30*rand(0)-40) with points linestyle 2")
       :using '("(binwidth*(floor(($1-binstart)/binwidth)+0.5)+binstart):(1.0)  smooth frequency with boxes linestyle 1")) file-name)))

(defun cat-hist (dataset &key col)
  "Accepts a dataset struct -> count of the categorical data"
  (let* ((group-name (string-downcase (symbol-name col)))
         (file-name (merge-pathnames *figures-dir*
                                     (concatenate 'string "count" group-name ".png")))
         (freq-data (column-freq (data dataset) :col col))
         (counts (mapcar #'second freq-data))
         (h (fd-bins counts)))
    (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (format t "binwidth = ~D~%" h)
      (format t "binstart = ~S~%" (* -2 h))
      (eazy-gnuplot:gp-setup :output file-name :terminal '(png size (1200 742) enhanced font "Verdana,8")
                             :key '(off)
                             :xlabel "orders"
                             :ylabel "count"
                             :xtic '(nomirror font ",8")
                             :ytic '(nomirror font ",8")
                             :tic '(nomirror out scale 0.75)
                             :border '(3 front linecolor rgb ("'#808080'") lt 1)
                             :boxwidth '(0.9*binwidth)
                             :style '(fill solid 0.5 noborder)
                             :style '(line 1 lt 1 lc rgb ("'#b20000'")))
      (eazy-gnuplot:plot
       (lambda ()
         (format t "~&~{~a ~%~}" counts))
       :using '("(binwidth*(floor(($1-binstart)/binwidth)+0.5)+binstart):(1.0)  smooth frequency with boxes linestyle 1")) file-name)
    file-name))

(defun cumfreq-plot (dataset &key col)
"Plot the cummulative frequency against rank for entity in dataset
Select entity with COL :mrn, :physician, or :location"
(let* ((data (data dataset))
       (cf-data (cummulative-frequency data :col col))
       (group-name (string-downcase (symbol-name col)))
       (file-name (merge-pathnames *figures-dir*
                                  (concatenate 'string "cdf" group-name ".png")))
       (test-name (test-name dataset)))
  (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :output file-name :terminal '(png size (1200 742) enhanced font "Verdana,8")
                             :key '(off)
                             :title test-name
                             :ylabel "frequency"
                             :xlabel "rank"
                             :ytic '(nomirror font ",8")
                             :xtic '(nomirror font ",8")
                             :tic '(nomirror out scale 0.75)
                             :border '(3 front linecolor rgb ("'#808080'") lt 1)
                             :style '(fill solid 0.5 noborder)
                             :style '(line 1 linetype 2 linewidth 4 linecolor rgb ("'#F46D43'")))
      (eazy-gnuplot:plot
       (lambda ()
         (format t "~&~{~d ~d ~%~}" '(0 0))
         (dolist (row cf-data)
           (format t "~&~{~d ~d ~%~}" row)))
       ;;:using '("1:2") :with '(lines linestyle 1)
       :using '("1:2 smooth bezier linestyle 1")) file-name)
    file-name))

(defun test-ts (dataset)
  "Plot a time series of test utilization"
  (let ((ym-freq (year-month-freq (data dataset)))
        (test-name (test-name dataset))
        (price (coerce (price dataset) 'single-float))
        (file-name (merge-pathnames *figures-dir* "testts.png")))
    (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :output file-name
                             :terminal '(png size (1200 742) enhanced font "Verdana,8")
                             :key '(off)
                             :timefmt "%Y-%m"
                             :xdata '(time)
                             :format '(x "%Y-%b")
                             :title test-name
                             :xlabel "month"
                             :ylabel "count"
                             :y2label "cost"
                             :ytic '(nomirror font ",8")
                             :xtic '(nomirror font ",8" rotate by -45)
                             :y2tic '(nomirror font ",8")
                             :link `(y2 via ,price * y inverse y / ,price)
                             :tic '(nomirror out scale 0.75)
                             :border '(11 front linecolor rgb ("'#808080'") lt 1)
                             :style '(fill solid 0.5 noborder)
                             :style '(line 1 linetype 2 linewidth 2 linecolor rgb ("'#8F5E99'")))
      (eazy-gnuplot:plot
       (lambda ()
         (dolist (row ym-freq)
           (format t "~&~{~a ~d ~%~}" row)))
       ;;:using '("1:2") :with '(lines linestyle 1)
       :using '("1:2 w linespoints linestyle 1")) file-name)
    file-name))

(defun label-scatterplot (data group-name xlabel ylabel)
  "Labeled scatter plot"
  (let* ((file-name (merge-pathnames *figures-dir*
                                     (concatenate 'string "xy" group-name ".png"))))
    (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :output file-name
                             :terminal '(png size (1200 742) enhanced font "Verdana,8")
                             ;;:terminal '(pdfcairo enhanced font "Verdana,8")
                             :key '(off)
                             :xlabel xlabel
                             :ylabel ylabel
                             :ytic '(nomirror font ",8")
                             :xtic '(nomirror font ",8")
                             :tic '(nomirror out scale 0.75)
                             :border '(3 front linecolor rgb ("'#808080'") lt 1)
                             :pointsize .5
                             :style '(line 1 linetype 1 lc rgb ("'#71637D'") lw 1)
                             :style '(line 2 linetype 3 lc rgb ("'#3288BD'") lw 1 pointtype 4))
                  (eazy-gnuplot:plot
       (lambda ()
         (dolist (row data)
           (destructuring-bind (&key label x-count y-count) row
             (format t "~s ~d ~d ~%" (string-capitalize label) x-count y-count))))
       :using '("2:3:1 with labels font 'verdana,8' point pt 6 offset char .5,.5")) file-name)
    file-name))

(defun xy-dataset (dataset group-col x-count y-count)
  (flet ((count-group (group)
           (let*((the-rest (rest group))
                 (tests (length the-rest))
                 (n (length
                     (remove-duplicates
                      (collect-column the-rest :col x-count)
                      :test #'equalp))))
             (list :label (first group)
                   :x-count n
                   :y-count tests))))
    (let* ((data (data dataset))
           (grouped-data (create-groups data :col group-col))
           (count-data (mapcar #'count-group grouped-data))
           (x-label (string-downcase (symbol-name x-count)))
           (y-label (string-downcase (symbol-name y-count)))
           (group-name (string-downcase (symbol-name group-col))))
      (label-scatterplot count-data group-name x-label y-label))))

(defun barchart-dataset (dataset &key col)
  "Accepts a dataset struct and :COLUMN -> a top n barchart"
  (let* ((name (string-downcase (symbol-name col)))
         (file-name (merge-pathnames *figures-dir*
                                     (concatenate 'string "barchart" name ".png")))
         (freq-data (column-freq (data dataset) :col col))
         (n (n dataset))
         (n-data (top-n freq-data n))
         (price (coerce (price dataset) 'single-float))
         (test-name (test-name dataset)))
    (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :output file-name :terminal '(png size (1200 742) enhanced font "Verdana,8")
                             :key '(off)
                             :title (substitute #\  #\_ test-name)
                             :xlabel ""
                             :ylabel "count"
                             :y2label "cost"
                             :yrange (list "[0:]")
                             :ytic '(nomirror font ",8")
                             :y2tic '(nomirror font ",8")
                             :link `(y2 via ,price * y inverse y / ,price)
                             :xtic '(textcolor rgb ("'#000000'") nomirror font ",8"  rotate by -45)
                             :tic '(nomirror out scale 0.75)
                             :border '(11 front linecolor rgb ("'#808080'") lt 1)
                             :boxwidth '(0.9)
                             :style '(data histogram)
                             :style '(fill solid 0.5 noborder)
                             :style '(line 1 lt 1 lc rgb ("'#4682b4'")))
      (eazy-gnuplot:plot
       (lambda ()
         (dolist (row n-data)
           (format t "~&~{~s ~a~%~}" row)))
       :using '(2 "xtic(1) with boxes linestyle 1")) file-name)
    file-name))

(defun barchart-normalized (dataset &key col)
  "Accepts a dataset struct and :COLUMN -> a top n barchart"
  (let* ((name (string-downcase (symbol-name col)))
         (file-name (merge-pathnames *figures-dir*
                                     (concatenate 'string "normalizedbarchart" name ".png")))
         (data (normalize-dataset (data dataset) :col col))
         (n (n dataset))
         (n-data (top-n data n))
         (test-name (test-name dataset)))
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output file-name :terminal '(png size (1200 742) enhanced font "Verdana,8")
                           :key '(off)
                           :title (substitute #\  #\_ test-name)
                           :xlabel ""
                           :ylabel "tests/patient"
                           :yrange (list "[0:]")
                           :ytic '(nomirror font ",8")
                           :xtic '(textcolor rgb ("'#000000'") nomirror font ",8"  rotate by -45)
                           :tic '(nomirror out scale 0.75)
                           :border '(3 front linecolor rgb ("'#808080'") lt 1)
                           :boxwidth '(0.9)
                           :style '(data histogram)
                           :style '(fill solid 0.5 noborder)
                           :style '(line 1 lt 1 lc rgb ("'#4682b4'")))
    (eazy-gnuplot:plot
     (lambda ()
       (dolist (row n-data)
         (format t "~&~{~s ~f ~a~%~}" row)))
     :using '( "2:xtic(1) with boxes linestyle 1")
     :using '( "0:($2+ 0.3):3 with labels font 'Verdana,8' textcolor rgb \"#808080\"")) file-name)
    file-name))

;;; Time series plots
(defun bubble-ts (dataset)
  (let* ((data (data dataset))
         (quant (quantitative-p dataset))
         (pats (patients dataset))
         (file-name (merge-pathnames *figures-dir* "top_mrns.csv")))
    (with-open-file (out file-name
                         :direction :output
                         :if-exists :supersede)
      (dolist (row data)
        (when (member (getf row :mrn) pats :test #'string=)
          (destructuring-bind (&key mrn provider specialty location verified-dt result clean-result lab units component-name) row
            (declare (ignore specialty lab units component-name))
            (format out "~a, ~s, ~s, ~a, ~a, ~a~%"
                    mrn provider location (local-time:format-timestring nil verified-dt) result clean-result)))))
    (uiop:run-program (list "/usr/bin/Rscript" (namestring (asdf:system-relative-pathname 'ut "timeseries.r")) (string quant))
                      :error-output (namestring (asdf:system-relative-pathname 'ut "R_errors.txt")))
    (merge-pathnames *figures-dir* "timeseries.png")))

(defun pat-ts (mrn &optional (n 20))
  (declare (optimize debug))
  (let* ((data (pat-query mrn))
         (freq-data (column-freq data :col :test-name))
         (tests (mapcar #'first (top-n freq-data n)))
         (file-name (merge-pathnames *figures-dir* "top_tests.csv")))
        (with-open-file (out file-name
                         :direction :output
                         :if-exists :supersede)
      (dolist (row data)
        (when (member (getf row :test-name) tests :test #'string=)
          (destructuring-bind (&key test-name provider location verified-dt) row
            (format out "~s, ~s, ~s, ~a ~%" test-name provider location (local-time:format-timestring nil verified-dt))))))
        (uiop:run-program (list "/usr/bin/Rscript" (namestring (asdf:system-relative-pathname 'ut "testseries.r")))
                          :error-output (namestring (asdf:system-relative-pathname 'ut "R_errors.txt")))
        (merge-pathnames *figures-dir* "testseries.png")))

;; (destructuring-bind (&key test-name provider location verified-dt) (first (pat-query-dbi "010695195"))
;;   (format t "~s, ~s, ~s, ~a ~%" test-name provider location (local-time:format-timestring nil verified-dt)))
;;; ANALYTE PLOTS END ::::::::::::::::::::::::::::::

;;; DIVISION PLOTS :::::::::::::::::::::::::::::::::
(defun xy-division (division-data group-col x-count y-count)
  (let ((data (merge-alists (aggregate-group division-data
                                             :group-col group-col
                                             :sum-col x-count)
                            (aggregate-group division-data
                                             :group-col group-col
                                             :sum-col y-count)))
        (x-label (string-downcase (symbol-name x-count)))
        (y-label (string-downcase (symbol-name y-count)))
        (name (remove #\- (string-downcase (symbol-name group-col)))))
    (label-scatterplot data name x-label y-label)))

;;; DIVISION PLOTS END :::::::::::::::::::::::::::::::::


;;; OVERVIEW PLOTS :::::::::::::::::::::::::::::::::
(defun barchart-group (group-data group-name &optional (n 20))
  (let* ((file-name (merge-pathnames *figures-dir*
                                     (concatenate 'string "barchart" group-name ".png")))
        (aggregate-data (top-n (aggregate-group group-data
                                            :group-col :group-field
                                            :sum-col :test-count) n)))
        (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :output file-name
                             :terminal '(png size (1200 742) enhanced font "Verdana,8")
                             :key '(off)
                             :title "";(substitute #\  #\_ group-name)
                             :xlabel ""
                             :ylabel "count"
                             :ytic '(nomirror font ",8")
                             :xtic '(textcolor rgb ("'#000000'") nomirror font ",8"  rotate by -45)
                             :tic '(nomirror out scale 0.75)
                             :border '(3 front linecolor rgb ("'#808080'") lt 1)
                             :boxwidth '(0.9)
                             :style '(data histogram)
                             :style '(fill solid 0.5 noborder)
                             :style '(line 1 lt 1 lc rgb ("'#4682b4'")))
      (eazy-gnuplot:plot
       (lambda ()
         (dolist (row aggregate-data)
           (format t "~&~{~s ~a~%~}" row)))
       :using '(2 "xtic(1) with boxes linestyle 1")) file-name)
        file-name))

(defun group-ts (group-data group-name &optional (n 20))
  "Create ts plot for:
AUTHORIZING_PROVIDER_NAME
RESULTING_SECTION_NAME
ORDERING_DEPARTMENT_NAME
MRN"
  (let* ((section nil)
         (previous-section nil)
         (aggregate-data (top-n (aggregate-group group-data
                                                 :group-col :group-field
                                                 :sum-col :test-count) n))
         (selection (mapcar #'first aggregate-data))
         (dir-string (namestring (asdf:system-relative-pathname 'ut "")))
         (ts-filename (concatenate 'string "outfile='" dir-string "figures/" group-name "ts.png'"))
         (ts-script (concatenate 'string dir-string "line_plot.gp")))
    (with-open-file (out (merge-pathnames *figures-dir* "group_ts_data.txt")
                         :direction :output
                         :if-exists :supersede)
      (dolist (row group-data)
        (when (member (getf row :group-field) selection :test #'string=)
          (let ((new-section (getf row :group-field)))
            (setf previous-section section)
            (setf section new-section)
            (if (equalp section previous-section)
                (destructuring-bind (&key group-field year-month test-count mrn-count) row
                  (declare (ignore mrn-count))
                  (format out "~S ~D ~D~%" group-field year-month test-count))
                (progn ;; else: start new header and print results
                  (format out "~%~%~S~%" (string-capitalize section))
                  (destructuring-bind (&key group-field year-month test-count mrn-count) row
                    (declare (ignore mrn-count))
                    (format out "~S ~D ~D~%" group-field year-month test-count))))))))
    (print ts-filename)
    (uiop:run-program (list "/usr/bin/gnuplot" "-e" ts-filename ts-script))))

(defun make-group-plots (&key
                           (group-list '("AUTHORIZING_PROVIDER_NAME" "RESULTING_SECTION_NAME" "ORDERING_DEPARTMENT_NAME" "MRN" "TEST_NAME"))  (n 20))
  (mapcar (lambda (group-name)
            (let ((group-data (group-query group-name)))
              (barchart-group group-data group-name n)
              (group-ts group-data group-name n))) group-list))
