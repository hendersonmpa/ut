;;; file utilities.lisp
(in-package :ut)

(defun prompt-read (prompt)
  "Create a prompt and read input"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun assoc* (key alist)
  "Return the cdr of the alist"
  (cdr (assoc key alist :test #'string=)))

(defun merge-alists (list-a list-b)
  "Merge two a-lists on common CAR string"
  (mapcar (lambda (row-a)
            (reduce #'append (mapcar #'list '(:label :x-count :y-count)
                                     (append  row-a  (assoc* (car row-a) list-b))))) list-a))

(defmacro filter-data (data tests)
  "Filter rows from p-list data by key and test ((:KEY (TEST KEY VAL))*) "
  (alexandria:with-gensyms (row)
    (let* ((key-list (mapcar #'first tests))
           (test-list (mapcar #'second tests))
           (arg-list (mapcar
                      (lambda (s)
                        (find-symbol (symbol-name s))) key-list)))                          ;convert keys to symbols
      `(flet ((filter-p (,row)
                (funcall (lambda ,arg-list
                           (and ,@(mapcar (lambda (arg) `(not (null ,arg))) arg-list)
                               ,@test-list))
                         ,@(mapcar (lambda (key) `(getf ,row ,key)) key-list))))
         (remove-if-not #'filter-p ,data)))))

(macroexpand-1 '(filter-data (data *dataset*)
                 ((:provider (string-equal provider "DABEE, VASSANT"))
                  (:location (string-equal location "pediatric medicine")))))

(defun make-func (f)
  #'(lambda (arg1 arg2)
       (funcall f arg1 arg2)))
(setq addr (make-func #'+))
(setq subr (make-func #'-))
(funcall addr 4 2)
(funcall subr 4 2)

(defmacro make-filter ()
  "Return a function to filter rows from p-list data by key and test ((:KEY (TEST KEY VAL))*) "
  `(lambda (row provider location)
     (and ,@(mapcar (lambda (key) `(not (null ,group))) keys)
          ,@(mapcar (lambda (arg group) `(string-equal  group)))
          (string-equal location place))
     ,@(mapcar (lambda (key) `(getf row ,key)) group-list)))

(defun filter-location (data location-list)
  (flet ((filter-p (row)
           (member (getf row :location) location-list
                   :test #'string-equal)))
    (remove-if-not #'filter-p data)))

;; (make-filter :provider :location)

;; (setq filter-t (make-filter :provider :location))
;; (funcall filter-t '(:MRN "008665473" :PROVIDER "DABEE, VASSANT" :SPECIALTY "Paediatrics"
;;                     :LOCATION "PEDIATRIC MEDICINE" :VERIFIED-DT @2015-07-28T1004
;;                     :RESULT "2.85" :CLEAN-RESULT 2.85 :LAB "TOH" :UNITS "mIU/L" :COMPONENT-NAME
;;                     "TSH") "DABEE, VASSANT" "pediatric medicine")


;; (member '(:MRN "008665473" :PROVIDER "DABEE, VASSANT" :SPECIALTY "Paediatrics"
;;           :LOCATION "PEDIATRIC MEDICINE" :VERIFIED-DT @2015-07-28T1004
;;           :RESULT "2.85" :CLEAN-RESULT 2.85 :LAB "TOH" :UNITS "mIU/L" :COMPONENT-NAME
;;           "TSH") '("pediatric medicine")
;;           :key #'(lambda (row) (getf row :location))
;;           :test #'string-equal)


(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'slot-definition-name (class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

(defun iqr (data)
  (let ((lower (stats:percentile data 25))
        (upper (stats:percentile data 75)))
    (- upper lower)))

(defun handler-parse-number (s)
  (handler-case (parse-number:parse-number s)
    (parse-error () nil)
    (type-error () nil)))

(defun handler-parse-universal (u)
  (handler-case (local-time:universal-to-timestamp u)
    (type-error () nil)))

;; (collect-column (data-set-data *data-set*) :column :result)
;; (number-list (sort (group-freq (data-set-data *data-set*) :pos #'third) #'> :key #'second) #'second)
;; (sort (group-freq (data-set-data *data-set*) :pos #'third) #'> :key #'second)

;;; Histogram Bin Width
;; The Freedman-Diaconis rule is very robust and works well in
;; practice. The bin-width is set to h=2∗IQR/n^1/3. So the number of
;; bins is (max-min)/h.

(defun fd-bins (data)
  "Determine the number of bin-width based on the data set"
  (let* ((max (apply #'max data))
         (min (apply #'min data))
         (n (length data))
         (iqr (max (iqr data) 1))       ;to prevent division by zero
         (h (* 2 (/ iqr (expt n (/ 1 3)))))
                                        ;(n-bins (ceiling (/ (- max min) h )))
         )
    h))

;; (defun cdf (data &key (nbins 10))
;;   (let* ((counter 0)
;;          (hist (statistics::histovalues data :nbins nbins))
;;          (sum (reduce #'+ hist :key #'third)))
;;     (mapcar #'(lambda (entry) (float (* 100 (/ (incf counter (third entry)) sum )) 1.0)) hist)))

;;; Generic query processing functions
(defun create-groups (data &key col (tests (list #'equalp)))
  "Group on COL using TESTS"
  (group-by:group-by-repeated data
                              :keys (list #'(lambda (row) (getf row col)))
                              :tests tests))
;; (create-groups (data-set-data *data-set*) :col :mrn :tests (list #'string-equal))

;;; count distinct mrn
(defun normalize-group (data &key group-col (denominator-col :mrn))
  "number of tests per unique mrn"
  (let ((grouped-data (create-groups data :col group-col)))
    (mapcar (lambda (group)
              (list (first group)
                    (/ (length (rest group))
                       (length (remove-duplicates
                                (collect-column (rest group) :col denominator-col)
                                :test #'equalp))))) grouped-data)))
