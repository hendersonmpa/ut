;;; file analyte.lisp
;;; Functions to filter, aggregate and format analyte level data
(in-package :ut)


(defun min-max-datetime (data)
  "Return the min and max datetime in the dataset"
  (let* ((dates (collect-column data :col :verified-dt))
         (max-timestring
          (local-time:format-timestring
           nil (apply #'local-time:timestamp-maximum dates)
           :format '(:long-month " " :ordinal-day " " :year)))
         (min-timestring
          (local-time:format-timestring
           nil (apply #'local-time:timestamp-minimum dates)
           :format '(:long-month " " :ordinal-day " " :year))))
    (list min-timestring max-timestring)))

(let ((freq (make-hash-table :test 'equalp)))
  (defun year-month-freq (data)
    "Create a frequency hashtable for year-month.
Used by TEST-TS"
    (flet ((get-year-month (row)
             (let* ((timestamp (getf row :verified-dt))
                    (year (local-time:timestamp-year timestamp))
                    (month (local-time:timestamp-month timestamp)))
               (format nil "~d-~2,'0d" year month))))
      (clrhash freq)
      (loop for row in data
         do (incf (the fixnum (gethash (get-year-month row) freq 0))))
      (let ((freqlist nil))
        (maphash #'(lambda (k v) (push (cons k v) freqlist)) freq)
        (loop for (k . v) in (sort freqlist  #'string< :key #'car)
           collect (list k v))))))

;;; Distinct entries
(defun count-distinct (data &key col)
  "Used in SUMMARY table"
  (let ((accum nil))
    (dolist (row data (length accum))
      (let ((item (getf row col)))
        (unless (member item accum :test #'string=)
          (push item accum))))))
;;(count-distinct (dataset-data *dataset*) :column :physician)

(defun cummulative-frequency (data &key col)
  "Used in CUMFREQ-PLOT"
  (let* ((freq-data (column-freq data :col col))
         (sorted-data (sort freq-data #'> :key #'second))
         (results (mapcar #'(lambda (row) (second row)) sorted-data))
         (total (reduce #'+ results)))
    (loop for x in results
       counting x into n
       sum x into y collect (list n (float (/ y total))))))

;;; Generic query processing functions
(defun create-groups (data &key col (tests (list #'equalp)))
  "Group on COL using TESTS"
  (group-by:group-by-repeated data
                              :keys (list #'(lambda (row) (getf row col)))
                              :tests tests))
;; (create-groups (dataset-data *dataset*) :col :mrn :tests (list #'string-equal))

;;; Normalize group counts by number of patients in group
(defun normalize-dataset (data &key col (denominator-col :mrn))
  "Number of tests per unique demominator entity: mrn, location provider
Used by BARCHART-NORMALIZED"
  (let ((grouped-data (create-groups data :col col)))
    (mapcar (lambda (group)
              (let*((the-rest (rest group))
                    (tests (length the-rest))
                    (n (length
                        (remove-duplicates
                         (collect-column the-rest :col denominator-col)
                         :test #'equalp))))
                (list (first group) (/ tests n) n))) grouped-data)))

;;; END DATASET FUNCTIONS ::::::::::::::::
