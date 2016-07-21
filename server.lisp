;;;; web-app.lisp
(in-package :ut)
;; Web server - Hunchentoot

(setf hunchentoot:*catch-errors-p* nil) ; T for production
(setf hunchentoot:*show-lisp-errors-p* t)
(setf hunchentoot:*show-lisp-backtraces-p* t)
(defparameter *http-port* 4242)
(defvar *my-acceptor* nil)
;; (defun start-server (port)
;;   (start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun start-server ()
  (unless *my-acceptor*
    (pushnew (hunchentoot:create-folder-dispatcher-and-handler
              "/static/" "~/lisp/site/ut/static/")
             hunchentoot:*dispatch-table* :test #'equal)
    (pushnew (hunchentoot:create-folder-dispatcher-and-handler
              "/figures/" "~/lisp/site/ut/figures/")
             hunchentoot:*dispatch-table* :test #'equal)
    (setf *my-acceptor*
          (hunchentoot:start (make-instance
                              'hunchentoot:easy-acceptor
                              :port *http-port*)))))
(defun stop-server ()
  (when *my-acceptor*
    (hunchentoot:stop *my-acceptor*)
    (setf hunchentoot:*dispatch-table*
          (last hunchentoot:*dispatch-table*))
    (setf *my-acceptor* nil)))

(defun restart-server ()
  (stop-server)
  (start-server))
;; Create our pages
(setf (html-mode) :html5)

(defmacro standard-page ((&key title script) &body body)
  "All pages on the site will use the following macro;
   less to type and a uniform look of the pages (defines the header
   and the stylesheet).
   The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(who:with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title ,title)
            (:meta :name "description" :content"")
            (:meta :http-equiv "X-UA-Compatible":content "IE=edge" )
            (:meta :name "viewport" :content "width=device-width, initial-scale=1")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/static/normalize.css")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/static/main.css")
            ;; (:link :type "text/css"
            ;;        :rel "stylesheet"
            ;;        :href "http://fonts.googleapis.com/css?family=Ubuntu")
            ;; (:link :type "text/css"
            ;;        :rel "stylesheet"
            ;;        :href "http://fonts.googleapis.com/css?family=Droid+Sans")
            ,(when script
                   `(:script :type "text/javascript"
                             (who:str ,script))))
           (:body
            (:header :role "banner"; Porph-screen header
                     (:img :src "/static/logo-cheo.png"
                           :alt "cheo-logo"
                           :class "logo")
                     (:span :class "strapline"
                            "CHEO Lab Test Utilization")
                     (:nav :role "navigation"
                           (:ul
                            (:li (:a :href "/all-docs" "Providers Overview"))
                            (:li (:a :href "/all-locs" "Locations Overview"))
                            (:li (:a :href "/all-pats" "Patients Overview"))
                            (:li (:a :href "/all-labs" "Lab Overview"))
                            (:li (:a :href "/all-tests" "Test Overview"))
                            (:li (:a :href "/select" "Select a test name"))

                            )))
            ,@body
            (:footer :role "contentinfo"
                     (:small (:p "Matthew P.A. Henderson"(:br)
                                 "Clinical Biochemist"(:br)
                                 "Department of Laboratory Medicine"(:br)
                                 "The Children's Hospital of Eastern Ontario" (:br)
                                 "e-mail: mathenderson at cheo dot on dot ca")
                             (:p "Created with Common Lisp"
                                 (:img :src "/static/LISP_logo.svg"
                                       :alt "Lisp Logo"
                                       :class "stamp"))))))))

(defmacro report-page ((&key title script) &body body)
  "All pages on the site will use the following macro;
   less to type and a uniform look of the pages (defines the header
   and the stylesheet).
   The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(who:with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title ,title)
            (:meta :name "description" :content"")
            (:meta :http-equiv "X-UA-Compatible":content "IE=edge" )
            (:meta :name "viewport" :content "width=device-width, initial-scale=1")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/static/normalize.css")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/static/main.css")
            ;; (:link :type "text/css"
            ;;        :rel "stylesheet"
            ;;        :href "http://fonts.googleapis.com/css?family=Ubuntu")
            ;; (:link :type "text/css"
            ;;        :rel "stylesheet"
            ;;        :href "http://fonts.googleapis.com/css?family=Droid+Sans")
            ,(when script
                   `(:script :type "text/javascript"
                             (who:str ,script))))
           (:body
            (:header :role "banner"; Porph-screen header
                     (:img :src "/static/logo-cheo.png"
                           :alt "cheo-logo"
                           :class "logo")
                     (:span :class "strapline"
                            "CHEO Lab Test Utilization")
                     (:nav :role "navigation"
                           (:ul
                            (:li (:a :href "/select" "Select a test name"))
                            (:li (:a :href "/summary" "Summary"))
                            (:li (:a :href "/docs" "Physicians"))
                            (:li (:a :href "/locs" "Locations"))
                            (:li (:a :href "/pats" "Patients"))
                            (:li (:a :href "/ts" "Time series"))
                            (:li (:a :href "/all-docs" "Back to overview")))))
            ,@body
            (:footer :role "contentinfo"
                     (:small (:p "Matthew P.A. Henderson"(:br)
                                 "Division of Biochemistry"(:br)
                                 "The Children's Hospital of Eastern Ontario" (:br)
                                 "e-mail: mathenderson at cheo dot on dot ca")
                             (:p "Created with Common Lisp"
                                 (:img :src "/static/LISP_logo.svg"
                                       :alt "Lisp Logo"
                                       :class "stamp"))))))))
