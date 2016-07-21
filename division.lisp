;;; file division.lisp
;;; Functions to facilitate division analysis

(in-package :ut)

(defgeneric make-location-dataset (dataset location-list)
  (:documentation "Filter dataset for locations"))

(defmethod make-location-dataset ((dt dataset) location-list)
  (with-accessors ((tn test-name)
                   (d data)
                   (qp quantitative-p)
                   (n n)
                   (pt patients)
                   (l lab)
                   (u units)
                   (p price)) dt
    (let ((subset (filter-location d location-list)))
      (make-instance 'dataset
                     :test-name tn
                     :data subset
                     :quantitative-p qp
                     :n n
                     :patients (get-pats subset n)
                     :lab l
                     :units u
                     :price p))))

(defun rename-plot (old-pathname new-pathname tag-string)
  "Move file to NEW-PATHNAME and add TAG-STRING"
  (let ((new-pathname
         (make-pathname :defaults new-pathname
                        :name (concatenate 'string (string-downcase tag-string)
                                           (pathname-name old-pathname)))))
    (rename-file old-pathname new-pathname)))

(defmacro plot-dataset-list (function-call dataset-list new-dir)
  "Applies FUNCTION-CALL to each dataset in DATASET-LIST and moves plot to NEW-DIR"
  `(mapcar (lambda (dataset)
             (rename-plot ,function-call ,new-dir
                          (test-name dataset))) ,dataset-list))

(defun division-analysis ()
  "Prompt for the specialty and locations"
  (let* (   ;(specialty-string (prompt-read "Please enter specialty"))
         ;;(specialty-keyword (intern (string-upcase specialty-string) "KEYWORD"))
         ;;(specialty-symbol (intern specialty-string))
         (division-name (prompt-read "Please enter the name of the division"))
         (division-dir (ensure-directories-exist
                        (make-pathname :defaults *figures-dir*
                                       :directory (append
                                                   (pathname-directory *figures-dir*)
                                                   (list division-name)))))
         (locations (prompt-read "Please enter a comma delimited list of locations"))
         (location-list (mapcar (lambda (str) (string-trim  '(#\Space) str))
                                (split-sequence:split-sequence #\, locations
                                                               :remove-empty-subseqs t)))
         (tests (prompt-read "Please enter a comma delimited list of analytes"))
         (test-list (mapcar (lambda (str) (string-trim  '(#\Space) str))
                            (split-sequence:split-sequence #\, tests
                                                           :remove-empty-subseqs t)))
         (division-data (division-counts location-list))
         (unfiltered-dataset-list (mapcar
                                   (lambda (test-name)
                                     (make-dataset test-name 20)) test-list))
         (dataset-list (mapcar
                        (lambda (dataset)
                          (make-location-dataset dataset location-list))
                        unfiltered-dataset-list)))
    (rename-plot (xy-division division-data :provider :mrn-count :test-count)
                 division-dir division-name)
    (rename-plot (xy-division division-data :test-name :mrn-count :test-count)
                 division-dir division-name)
    (rename-plot (xy-division division-data :specialty :mrn-count :test-count)
                 division-dir division-name)
    (rename-plot (barchart-group division-data :provider "provider")
                 division-dir division-name)
    (rename-plot (barchart-group division-data :test-name "test")
                 division-dir division-name)
    (rename-plot (barchart-group division-data :specialty "specialty")
                 division-dir division-name)
    (plot-dataset-list (barchart-dataset dataset :col :provider)  dataset-list division-dir)
    (plot-dataset-list (bubble-ts dataset)  dataset-list division-dir)
    (plot-dataset-list (pat-ts (first (patients dataset)))  dataset-list division-dir)
    (plot-dataset-list (xy-dataset dataset :provider :mrn :test) dataset-list division-dir)))

;;Respiratory Virus PCR Panel
;;Respiratory Virus Triplex PCR

;; (defparameter *dataset-list*
;;   (mapcar (lambda (dataset)
;;      (make-location-dataset dataset
;;                             '("NN Intensive Care Unit" "Intensive Care Unit")))
;;    (mapcar (lambda (test-name)
;;       (make-dataset test-name 20)) '("tsh" "creatinine"))))

;; (macroexpand-1 '(plot-dataset-list (barchart-dataset dataset :col :provider)  *dataset-list* *figures-dir*))

;;(plot-dataset-list (barchart-dataset dataset :col :provider)  *dataset-list* *figures-dir*)


;; (division-analyte '("4 North" "4 East" "4 West") "Creatinine")
;; (defparameter *division-data* (division-counts "paediatrics" '("4 North")))
;; (defparameter *division-data* (division-counts "endocrinology" '("Endocrinology")))
;; (defparameter *division-data* (division-counts "paediatrics" '("NN Intensive Care Unit" "Intensive Care Unit")))

;; (defparameter *div-counts* (division-counts '("NN Intensive Care Unit")))

;; (defun make-new-data-set (dataset location)
;;   )

;; (let ((division-name "emergency")
;;       (division-data *div-counts*))
;;   (to-div-dir (xy-division division-data :provider :mrn-count :test-count) division-name)
;;    (to-div-dir (xy-division division-data :test-name :mrn-count :test-count) division-name)
;;    (to-div-dir (xy-division division-data :specialty :mrn-count :test-count) division-name)
;;    (to-div-dir (barchart-group division-data :provider "provider") division-name)
;;    (to-div-dir (barchart-group division-data :test-name "test") division-name)
;;    (to-div-dir (barchart-group division-data :specialty "specialty") division-name))
