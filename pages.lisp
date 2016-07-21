;;; file pages.lisp
(in-package :ut)

(defun test-sub (str &optional (test-name (test-name *dataset*))
                       (pattern "ANALYTE-NAME"))
  "Substitute in the correct test name"
  (cl-ppcre:regex-replace-all
   pattern str test-name))

(define-easy-handler (all-docs :uri "/all-docs") ()
  (standard-page (:title "Physicians")
    (:h3 (str "Overview of test volume by physician for one year."))
    (:p (:img :src "/figures/AUTHORIZING_PROVIDER_NAME_bar.png" :alt "barchar"  :align "middle"))
    (:p (:img :src "/figures/AUTHORIZING_PROVIDER_NAME_ts.png" :alt "timeseries" :align "middle"))
    (:p (:a :href "/all-locs" "Next: Locations"))))

(define-easy-handler (all-locs :uri "/all-locs") ()
  (standard-page (:title "Ordering Locations")
    (:h3 (str "Overview of test volume by ordering location for one year"))
    (:p (:img :src "/figures/ORDERING_DEPARTMENT_NAME_bar.png" :alt "barchart"))
    (:p (:img :src "/figures/ORDERING_DEPARTMENT_NAME_ts.png" :alt "timeseries"))
    (:p (:a :href "/all-pats" "Next: Patients"))))

(define-easy-handler (all-pats :uri "/all-pats") ()
  (standard-page (:title "Patients")
    (:h3 (str "Overview of test volume by patient for one year"))
    (:p (:img :src "/figures/MRN_bar.png" :alt "barchart"))
    (:p (:img :src "/figures/MRN_ts.png" :alt "timeseries"))
    (:p (:a :href "/all-labs" "Next: Labs"))))

(define-easy-handler (all-labs :uri "/all-labs") ()
  (standard-page (:title "Labs")
    (:h3 (str "Overview of test volume by lab section for one year"))
    (:p (:img :src "/figures/RESULTING_SECTION_NAME_bar.png" :alt "barchart"))
    (:p (:img :src "/figures/RESULTING_SECTION_NAME_ts.png" :alt "timeseries"))
    (:p (:a :href "/all-tests" "Next: Tests"))))

(define-easy-handler (all-tests :uri "/all-tests") ()
  (standard-page (:title "Tests")
    (:h3 (str "Overview of test volume by analyte for one year"))
    (:p (:img :src "/figures/TEST_NAME_bar.png" :alt "barchart"))
    (:p (:img :src "/figures/TEST_NAME_ts.png" :alt "timeseries"))
    (:p (:a :href "/select" "Next: Select a Test"))))

(define-easy-handler (select :uri "/select") ()
  (standard-page (:title "Utilization Report")
    (:h3 "Analyte Report")
    (:p "Select the test name")
    (:datalist :id "tests"
               (dolist (test *test-list*)
                 (cl-who:htm
                  (:option :value test))))
    (:form :method :post :enctype "multipart/form-data"
           :action "query" ;; What happens when you submit
           (:p "Test name"
                  (:input :list "tests" :type :text
                       :name "test-name" :class "txt" :required "required"))
           (:p "Top n"
               (:input :type :number :title "top n" :name "n"
                       :value 20 :min 1 :class "txt" :required "required"))
           (:p (:input :type :submit :value "Submit" :class "btn")))))

(define-easy-handler (query :uri "/query") (test-name n)
  (make-dataset test-name n)
  (if (data *dataset*)
      (redirect "/summary")
      (redirect "/select")))

(define-easy-handler (summary :uri "/summary") ()
  (if (quantitative-p *dataset*)
      (histogram *dataset*)
      (barchart-dataset *dataset* :col :result))
  (test-ts *dataset*)
  (let* ((data (data *dataset*))
         (dates (min-max-datetime data)))
    (report-page (:title "Summary")
      (:h3 (str (test-sub "Summary of ANALYTE-NAME use and results")))
      (:p (str (test-sub "An overview of ANALYTE-NAME utilization over
        the indicated time frame is presented in Table 1. A graphical
        summary of ANALYTE-NAME results for all orders during this
        time is presented in Figure 1.")))
      (:table (:tr
               (:th "Group" )
               (:th "Value" ))
              (:tr
               (:td "Start Date")
               (:td (str (first dates))))
              (:tr
               (:td "End Date")
               (:td (str (second dates))))
              (:tr
               (:td "Test Count")
               (:td (str (length data))))
              (:tr
               (:td "Physician Count")
               (:td (str (count-distinct data :col :provider))))
              (:tr
               (:td "Location Count")
               (:td (str (count-distinct data :col :location))))
              (:tr
               (:td "Patient Count")
               (:td (str (count-distinct data :col :mrn))))
              (:tr
               (:td "Performing Lab")
               (:td (str (lab *dataset*)))))
      (if (quantitative-p *dataset*)
          (cl-who:htm (:img :src "/figures/histogram.png" :alt "distribution of results"))
          (cl-who:htm (:img :src "/figures/barchartresult.png" :alt "distribution of results")))
      (:img :src "/figures/test_ts.png" :alt "test timeseries")
      (:p (:a :href "/docs" "Next: Providers")))))

(define-easy-handler (docs :uri "/docs") ()
  (cat-hist *dataset* :col :provider)
  (barchart-dataset *dataset* :col :provider)
  (barchart-normalized *dataset* :col :provider)
  (xy-dataset *dataset* :provider :mrn :test)
  (cumfreq-plot *dataset* :col :provider)
  (report-page (:title "Physicians")
    (:h3 (str (test-sub "Overview of physician orders for ANALYTE-NAME")))
    (:p (str (test-sub "The orders for ANALYTE-NAME were grouped by
               ordering physician in order to identify physicians with
               the greatest order frequency. A histogram of the number
               of ANALYTE-NAME orders by a given physician is
               presented in Figure 1. The physicians with the greatest
               number of orders are presented in Figure 2. Physicians
               with the highest number of orders per patient (MRN) are
               showed in Figure 3.Figure 4 shows the cumulative
               frequency of ANALYTE-NAME orders.")))
    (:p (:h3 "Figure 1")
        (:img :src "/figures/countprovider.png" :alt "count plot"))
    (:p (:h3 "Figure 2")
        (:img :src "/figures/barchartprovider.png" :alt "barchart"))
    (:p (:h3 "Figure 3")
        (:img :src "/figures/normalizedbarchartprovider.png" :alt "normalized barchart"))
    (:p (:h3 "Figure 4")
        (:img :src "/figures/xyprovider.png" :alt "normalized barchart"))
    (:p (:h3 "Figure 5")
        (:img :src "/figures/cdfprovider.png" :alt "cdf"))
    (:p (:a :href "/locs" "Next: Locations"))))

(define-easy-handler (locs :uri "/locs") ()
  (cat-hist *dataset* :col :location)
  (barchart-dataset *dataset* :col :location)
  (barchart-normalized *dataset* :col :location)
  (xy-dataset *dataset* :location :mrn :test)
  (cumfreq-plot *dataset* :col :location)
  (report-page (:title "Locations")
    (:h3 (str (test-sub "Overview of ANALYTE-NAME orders by location")))
    (:p (str (test-sub "The orders for ANALYTE-NAME were grouped by
location in order to identify locations in the hospital with the
greatest order frequency. A histogram of the number of ANALYTE-NAME
orders from a given location is presented in Figure 1. The locations
with the greatest number of orders are presented in Figure
2. Locations with the highest number of orders per patient (MRN) are
shown in Figures 3 and 4. Figure 5 shows the cumulative frequency of
ANALYTE-NAME orders.")))
    (:p (:h3 "Figure 1")
        (:img :src "/figures/countlocation.png" :alt "count plot"))
    (:p (:h3 "Figure 2")
        (:img :src "/figures/barchartlocation.png" :alt "barchart"))
    (:p (:h3 "Figure 3")
        (:img :src "/figures/normalizedbarchartlocation.png" :alt "normalized barchart"))
    (:p (:h3 "Figure 4")
        (:img :src "/figures/xylocation.png" :alt "normalized barchart"))
    (:p (:h3 "Figure 5")
        (:img :src "/figures/cdflocation.png" :alt "cdf"))
    (:p (:a :href "/spec" "Next: Specialty"))))


(define-easy-handler (spec :uri "/spec") ()
  (cat-hist *dataset* :col :specialty)
  (barchart-dataset *dataset* :col :specialty)
  (barchart-normalized *dataset* :col :specialty)
  (xy-dataset *dataset* :specialty :mrn :test)
  (cumfreq-plot *dataset* :col :specialty)
  (report-page (:title "Specialties")
    (:h3 (str (test-sub "Overview of ANALYTE-NAME orders by specialty")))
    (:p (str (test-sub "The orders for ANALYTE-NAME were grouped by
specialty in order to identify specialties in the hospital with the
greatest order frequency. A histogram of the number of ANALYTE-NAME
orders from a given specialty is presented in Figure 1. The specialties
with the greatest number of orders are presented in Figure
2. Physicians with the highest number of orders per patient (MRN) are
shown in Figures 3 and 4. Figure 5 shows the cumulative frequency of
ANALYTE-NAME orders.")))
    (:p (:h3 "Figure 1")
        (:img :src "/figures/countspecialty.png" :alt "count plot"))
    (:p (:h3 "Figure 2")
        (:img :src "/figures/barchartspecialty.png" :alt "barchart"))
    (:p (:h3 "Figure 3")
        (:img :src "/figures/normalizedbarchartspecialty.png" :alt "normalized barchart"))
    (:p (:h3 "Figure 4")
        (:img :src "/figures/xyspecialty.png" :alt "normalized barchart"))
    (:p (:h3 "Figure 5")
        (:img :src "/figures/cdfspecialty.png" :alt "cdf"))
    (:p (:a :href "/pats" "Next: Patients"))))

(define-easy-handler (pats :uri "/pats") ()
  (cat-hist *dataset* :col :mrn)
  (barchart-dataset *dataset* :col :mrn)
  (cumfreq-plot *dataset* :col :mrn)
  (report-page (:title "Patients")
    (:h3 (str (test-sub "Overview of ANALYTE-NAME orders by patient")))
    (:p (str (test-sub "The orders for ANALYTE-NAME were grouped by
patient in order to identify patients with the greatest order
frequency.  A histogram of the number of ANALYTE-NAME orders for a
given patient is presented in Figure 1. The patients with the greatest
number of orders are presented in Figure 2. Figure 3 shows the
cumulative frequency of ANALYTE-NAME orders.")))
    (:p (:h3 "Figure 1")
        (:img :src "/figures/countmrn.png" :alt "count plot"))
    (:p (:h3 "Figure 2")
        (:img :src "/figures/barchartmrn.png" :alt "barchart"))
    (:p (:h3 "Figure 3")
        (:img :src "/figures/cdfmrn.png" :alt "cdf"))
    (:p (:a :href "/ts" "Next: Time Series"))))

(define-easy-handler (ts :uri "/ts") ()
  (bubble-ts *dataset*)
  (report-page (:title "Time Series")
    (:h3 (str (test-sub "Time Series of ANALYTE-NAME orders")))
    (:p (:img :src "/figures/timeseries.png" :alt "timeseries"))
    (:datalist :id "Patients"
               (dolist (patient (patients *dataset*))
                 (cl-who:htm
                  (:option :value patient))))
    (:form :method :post :enctype "multipart/form-data"
           :action "tests" ;; What happens when you submit
           (:p "MRN"
               (:input :list "Patients" :type :text :name "mrn" ))
           (:p (:input :type :submit :value "Submit" :class "btn")))))

(define-easy-handler (tests :uri "/tests") (mrn)
  (pat-ts mrn 40)
  (report-page (:title "Associated Tests")
    (:h3 (str (format nil "Most frequent tests MRN: ~A" mrn)))
    (:p (:img :src "/figures/testseries.png" :alt "testseries"))
    (:a :href "/ts" "Go back to timeseries")))
    ;; (:img :src "/figures/count.png" :alt "count plot")
    ;; (:p (:img :src "/figures/barchart.png" :alt "barchart"))))

(hunchentoot:define-easy-handler (print-param :uri "/print-param") ()
  (let ((params (hunchentoot:post-parameters*)))
    (standard-page (:title "print params")
      (:p (str params))
      (:p (str (mapcar (lambda (x) (type-of (cdr x))) params)))
      (:p (str (mapcar (lambda (x) (type-of (cdr x))) params))))))
