;;; file dataset.lisp
;;; defclass for the data set object
;;; functions to create a defclass object
(in-package :ut)

(defclass dataset ()
  ((test-name :accessor test-name :initarg :test-name :initform "" :type string)
   (data :accessor data :initarg :data :initform nil :type list)
   (quantitative-p :accessor quantitative-p :initarg :quantitative-p :initform nil :type boolean)
   (n :accessor n :initarg :n :initform 20 :type integer)
   (patients :accessor patients :initarg :patients :initform nil :type list)
   (lab :accessor lab :initarg :lab :initform "" :type string)
   (units :accessor units :initarg :units :initform "" :type string)
   (price :accessor price :initarg :price :initform 1.0 :type float))
  (:documentation "parent dataset object"))

(defun collect-column (data &key col)
    "Return a list containing the data in COLUMN for each row for the dataset"
  (mapcar #'(lambda (row)
              (getf row col)) data))

(let ((freq (make-hash-table :test 'equalp)))
  (defun column-freq (data &key (col :mrn))
    "Create a frequency hashtable at column :COL in data"
    (clrhash freq)
    (loop for row in data
       do (incf (the fixnum (gethash (getf row col) freq 0))))
    (let ((freqlist nil))
      (maphash #'(lambda (k v) (push (cons v k) freqlist)) freq)
      (loop for (k . v) in (sort freqlist  #'> :key #'car)
         collect (list v k)))))

(defun top-n (data n)
  "Find the top n most frequent entries in out-put from column-freq"
  (let* ((sorted (sort data #'> :key #'second))
         (n-data (if (>= (length sorted) n)
                     (subseq sorted 0 n)
                     sorted))
         (accum nil))
    (dolist (row n-data (nreverse accum))
      (if (null (car row))
          (push (cons "empty" (rest row)) accum)
          (push row accum)))))

(defun get-pats (data n)
  "Top n list of patients: used by CREATE-DATASET"
  (let ((freq-data (column-freq data :col :mrn)))
    (mapcar #'first (top-n freq-data n))))

(defparameter *dataset* nil
  "This will hold the DATASET object")

(defun make-dataset (test-name n)
  "Create a DATASET object from TEST-QUERY results"
  (let* ((data (test-query test-name))
         (num (if (numberp n) n
                  (parse-integer n :junk-allowed nil)))
         (nil-fraction (/ (count nil (collect-column data :col :clean-result))
                          (max (length data) 1)))
         (lab (getf (car data) :lab))
         (units (getf (car data) :units))
         (price (gethash test-name *price-ht*)))
    (setf *dataset* (make-instance 'dataset
                                   :test-name (string-upcase test-name)
                                   :data data
                                   :quantitative-p (if (< nil-fraction .7)
                                                       t nil)
                                   :n num
                                   :patients (get-pats data num)
                                   :lab lab
                                   :units units
                                   :price (if price price 1.0)))))

;; (sb-sprof:with-profiling (:max-samples 1000
;;                                        :mode :cpu
;;                                        :report :flat)
;;   (create-dataset "tsh" 20))
