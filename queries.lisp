;;; file queries.lisp
(in-package :ut)

;;(clsql:file-enable-sql-reader-syntax)
;;(clsql:disable-sql-reader-syntax)
;;(clsql:enable-sql-reader-syntax)
;;(clsql-sys:sql [response_date date]) ;;to see the statement

(defparameter *pwd* (prompt-read "Password"))

(defparameter *connection*
  (dbi:connect :mysql
               :database-name "lab"
               :username "mpah"
               :password *pwd*))

(defun query-sub (str test-name pattern)
  "Pseudo parameterised queries"
  (cl-ppcre:regex-replace-all
   pattern str test-name))

(defun test-list ()
  "To create a test drop down box"
  (let ((file-name (merge-pathnames *figures-dir* "test-list"))
        (data (dbi:execute
              (dbi:prepare *connection* "select distinct(test_name) from result order by component_name"))))
    (cl-store:store
     (loop for (k1 test-name) = (dbi:fetch data)
        while test-name
        collect test-name)  file-name)))

(defvar *test-list* (cl-store:restore (merge-pathnames *figures-dir* "test-list")))

(defun price-query ()
  "get price data used to create price hashtable"
  (let ((data (dbi:execute
               (dbi:prepare *connection* "SELECT result.test_name, price.price
FROM (SELECT distinct(test_name), test_id FROM result) result
LEFT JOIN price
ON result.test_id = price.test_id"))))
    (loop for (k1 test-name k2 price) = (dbi:fetch data)
       while test-name
       collect (list :test-name  test-name
                     :price price))))

(defun store-price-ht ()
  "Create a price hashtable from price-query output"
  (let ((ht (make-hash-table :test 'equalp)))
    (loop for row in (price-query)
       do (setf (gethash (getf row :test-name) ht nil) (getf row :price))
       finally (cl-store:store ht
                               (merge-pathnames *figures-dir* "price-ht")))))

(defparameter *price-ht* (cl-store:restore (merge-pathnames *figures-dir* "price-ht")))

(defun table-exists-p (table-name)
  (dbi:fetch-all
   (dbi:execute
    (dbi:prepare *connection* "SHOW TABLES like ?") table-name)))

(defun create-test-view ()
  (dbi:execute (dbi:prepare *connection* "CREATE OR REPLACE VIEW test AS
        (SELECT r.MRN, r.TEST_NAME, r.COMPONENT_NAME, r.RESULT, r.COMPONENT_UNITS, r.VERIFIED_DATETIME, r.ORDERING_DEPARTMENT_NAME,
        r.AUTHORIZING_PROVIDER_NAME, r.RESULTING_SECTION_NAME, p.SPECIALTY from result as r
       JOIN provider as p
       ON p.id = r.authorizing_provider_id
        where VERIFIED_DATETIME >= DATE(NOW()-INTERVAL 1 YEAR) group by TEST_NAME, ACCESSION)")))

(defun test-query (test-name &key (new nil) (anonymous nil))
  "get data for the analyte report: used to create data-set object"
  (let ((query (dbi:prepare *connection* "select mrn, authorizing_provider_name, specialty,
ordering_department_name, verified_datetime, result, resulting_section_name, component_units, component_name from test
where test_name = ?")))
    (if (or new (not (table-exists-p "test")))
        (create-test-view))
    (let ((data (dbi:execute query test-name)))
      (loop for (k1 mrn k2 provider k3 specialty k4 location k5 verified-dt k6 result k7 lab k8 units k9 component-name) = (dbi:fetch data)
         while (or mrn provider location)
         collect (list :mrn mrn
                       :provider (if anonymous
                                     (write-to-string (sxhash provider))
                                     provider)
                       :specialty specialty
                       :location location
                       :verified-dt (handler-parse-universal verified-dt)
                       :result result
                       :clean-result (handler-parse-number result)
                       :lab lab
                       :units units
                       :component-name component-name)))))

(defun pat-query (mrn)
  "all tests for mrn used in PAT-TS"
  (let* ((query (dbi:prepare *connection* "select test_name, authorizing_provider_name,
ordering_department_name, verified_datetime from result
where mrn = ?
group by test_name , ordered_datetime"))
         (data (dbi:execute query mrn)))
    (loop for (k1 test-name k2 provider k3 location k4 verified-dt) = (dbi:fetch data)
       while (or test-name provider location)
       collect (list :test-name test-name
                     :provider provider;; (sxhash provider)
                     :location location
                     :verified-dt (handler-parse-universal verified-dt)))))

(defun group-query (group-field &key (new nil))
    "Returns aggregate data for group"
    (let ((query-statement (dbi:prepare *connection*
            (query-sub "select GROUP-FIELD, date_format(VERIFIED_DATETIME,'%Y-%m')
 yearmonth, count(GROUP-FIELD) as test_count, count(distinct mrn) as mrn_count
 from test group by GROUP-FIELD, yearmonth
 order by GROUP-FIELD" group-field "GROUP-FIELD"))))
      (if (or new (not (table-exists-p "test")))
          (create-test-view))
      (let ((data (dbi:execute query-statement)))
        (loop for (k1 group-field k2 year-month k3 test-count k4 mrn-count) = (dbi:fetch data)
           while (or group-field year-month test-count)
           collect (list :group-field group-field
                         :year-month year-month
                         :test-count test-count
                         :mrn-count mrn-count)))))

(defun make-group-results-file (&optional (group-name "ORDERING_DEPARTMENT_NAME"))
  (cl-store:store
   (group-query group-name :new T)
   (merge-pathnames *figures-dir* "group-results")))

;;(make-group-results-file)

(defparameter *group-results*
  (cl-store:restore
   (merge-pathnames *figures-dir* "group-results"))
  "group-query output test data")

;;; Division Queries  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun division-counts (location-list &key (new nil))
  "Returns provider count data"
  (let* ((location-string (format nil "(~{~s~^,~})" location-list))
         (query-string  (query-sub "select authorizing_provider_name, specialty, test_name, count(test_name) as test_count, count(distinct mrn) as mrn_count
from test
where ordering_department_name in LOCATION-STRING
group by authorizing_provider_name, test_name"
                                   location-string "LOCATION-STRING"))
         (query (dbi:prepare *connection* query-string))
         (data (dbi:execute query)))
    (if (or new (not (table-exists-p "test")))
        (create-test-view))
      (loop for (k1 provider k2 specialty k3 test-name k4 test-count k5 mrn-count) = (dbi:fetch data)
         while (and provider test-name test-count mrn-count)
         collect (list :provider provider
                       :specialty specialty
                       :test-name test-name
                       :test-count test-count
                       :mrn-count mrn-count))))
