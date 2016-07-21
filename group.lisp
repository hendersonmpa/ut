;;; file group.lisp
;;; Functions to filter, aggregate and format group-query and division-query data

(in-package :ut)

;;; GROUP-QUERY FUNCTIONS :::::::::::::::::
;;; Process GROUP-QUERY output
(defun aggregate-group (data &key group-col sum-col)
  "(aggregate-group *group-results* :sum-col :mrn-count)"
  (flet ((map-groups (grouped-data)
           (mapcar (lambda (group)
                     (list (first group)
                           (apply #'+ (mapcar
                                       (lambda (row)
                                         (getf row sum-col))(rest group)))))
                   grouped-data)))
    (let* ((grouped-data (create-groups data :col group-col :tests (list #'string-equal)))
           (clean-data (remove-if #'null grouped-data :key #'car)))
      (sort (map-groups clean-data) #'> :key #'second))))

;;; Normalize data by number of associated mrns
;;; Files for testing
(defparameter *mrn-count*
  (with-open-file (in (merge-pathnames *figures-dir* "mrn-count")
                      :direction :input
                      :if-does-not-exist nil)
    (read in nil nil)) "Patient counts for phycians and locations")

;;; Test price and cost
(defvar *cost* (make-hash-table :test 'equalp ))

(defun test-cost (data &optional (price-ht *price*))
  "Accept ((test-name volume)..) return ((test-name cost)...)"
  (clrhash *cost*)
  (loop for (test-name test-volume) in data
     for test-price = (gethash test-name price-ht)
     when test-price ;; some test have no price data
       do (setf (gethash test-name *cost*) (* test-price test-volume)))
  (let ((costlist nil))
    (maphash #'(lambda (k v) (push (cons v k) costlist)) *cost*)
    (loop for (k . v) in (sort costlist  #'> :key #'car)
       collect (list v k))))

(defun price-list (test-list price-ht)
  (dolist (test test-list)
    (format t "Test: ~A ~C price: ~D~%" test #\tab (gethash test price-ht))))

;;(step (test-cost (top-n (aggregate-by *group-results* #'+ :keys (list #'first)) 50)))

(defun normalize-group (group-results &key group-key mrn-key (mrn-count *mrn-count*))
  ;; TODO: get mrn count denominator and test count numerator from one data set
  "Normalize GROUP-QUERY output"
  (let ((denominator-alist (aggregate-group mrn-count #'+
                                         :keys mrn-key
                                         :tests (list #'string-equal)))
        (numerator-alist (aggregate-by group-results #'+
                                         :keys group-key
                                         :tests (list #'string-equal)))
        (accum nil))
    (dolist (row numerator-alist (nreverse accum))
      (let* ((denominator (second (assoc (first row) denominator-alist :test #'string-equal)))
             (normalized (float (/ (second row) denominator))))
        (push (list (first row) normalized) accum)))))
;; (normalize-group *group-results* :mrn-key (list #'second))

;;; GROUP-QUERY FUNCTIONS END:::::::::::::::::
