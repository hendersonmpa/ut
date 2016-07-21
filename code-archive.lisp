;;; Old code I can get ride of yet

(defun svg-term (&key
                   (size (cons 800 600))
                   (font-face "arial")
                   font-point-size
                   ;; fontsize can be :tiny, :small, :medium, :large, :giant
                   (font-size :medium)
                   transparent
                   interlace
                   truecolor
                   (rounded t)
                   enhanced
                   colors)
  "Generates the type string for a svg terminal with options"
  (string-downcase
   (apply #'cl-ana.plotting::join-strings
          (cl-ana.plotting::intersperse
           " "
           (remove-if-not
            #'identity
            (alexandria:flatten
             (list "svg"
                   (when size
                     (list 'size (car size) "," (cdr size)))
                   (when font-face
                     (list "font"
                           font-face
                           (when font-point-size
                             font-point-size)))
                   (when (not font-point-size)
                     font-size)
                   (when transparent
                     "transparent")
                   (when interlace
                     "interlace")
                   (when truecolor
                     "truecolor")
                   (when (not rounded)
                     "butt")
                   (when enhanced
                     "enhanced")
                   (when colors
                     colors))))))))

;;; Histogram functions
(defun create-hist (data)
  (let* ((min (apply #'min data))
         (max (apply #'max data))
         (bins (fd-bins data))
         (hist (cl-ana.histogram:make-contiguous-hist
                `((:name "x"
                         :low ,min
                         :high ,max
                         :nbins ,bins))
                :empty-bin-value 0
                :default-increment 1)))
    (dolist (x data hist)
      (cl-ana.histogram:hist-insert hist x))))

(defun histogram (data-set)
  (let* ((file-name (merge-pathnames *figures-dir* "histogram.svg"))
         (clean-data (number-list (data-set-data data-set) #'fifth))
         (hist (create-hist clean-data))
         (test-name (data-set-test-name data-set))
         (units (data-set-units data-set)))
    (declare (ignorable units file-name))
    (cl-ana.plotting:restart-gnuplot-sessions)
    (cl-ana.plotting:draw hist
                          :line-args (list
                                      :title test-name
                                      :color "#71637D") ;garden plum
                          :plot-args (list
                                      :x-title units
                                      :y-title "count")
                          :page-args (list :terminal (svg-term :size (cons 800 600)
                                                               :font-face "verdana"
                                                               :font-size :small
                                                               :enhanced t)
                                           :output file-name))))
                          ;; :page-args (list :terminal (cl-ana.plotting:qt-term :size (cons 800 600)))  /home/mpah/lisp/site/lab:


(defun cat-hist (data-set &key (pos #'first))
  "Accepts a data-set struct -> count of the categorical data"
  (let* ((file-name (merge-pathnames *figures-dir* "count.svg"))
         (freq-data (group-freq (data-set-data data-set) :pos pos))
         (counts (mapcar #'(lambda (row) (second row)) freq-data))
         (hist (create-hist counts)))
    (declare (ignorable file-name))
    (cl-ana.plotting:restart-gnuplot-sessions)
    (cl-ana.plotting:draw hist
                          :line-args (list
                                      :title (data-set-test-name data-set)
                                      :color "#b20000")
                          :plot-args (list
                                      :x-title ""
                                      :y-title "count")
                          :page-args (list :terminal (svg-term :size (cons 800 600)
                                                               :font-face "verdana"
                                                               :font-size :small
                                                               :enhanced t)
                                           :output file-name)
                          ;; :page-args (list :terminal (cl-ana.plotting:qt-term :size (cons 800 600)))
                          )))

;;; Barchart Functions
(defun barchart-results (data)
  "Accepts a list of form ((string number) (string number)..) "
  (do ((p 1 (1+ p))
       (f-list data (cdr f-list))
       (tic-labels nil (cons (list :name (string-capitalize (caar f-list))
                                   :position p) tic-labels))
       (data-points nil (cons (cons p (cadar f-list)) data-points)))
      ((null (cdr f-list)) (values data-points tic-labels))))

(defun barchart-cmd (data test-name file-name n)
  "Accepts the components of a data-set struct"
  (declare (ignorable file-name))
   (multiple-value-bind (data-points tic-labels) (barchart-results data)
      (cl-ana.plotting:draw data-points
                            :line-args (list :title test-name
                                             :style "boxes"
                                             :color "#4682b4")
                            :plot-args (list :x-range (cons 0 n)
                                             :y-range (cons 0 "")
                                             :x-tics (cl-ana.plotting:tics
                                                      :manual-labels tic-labels
                                                      :rotate "by -45"))
                            :page-args (list :terminal (svg-term :size (cons 800 600)
                                                                 :font-face "verdana"
                                                                 :font-size :small
                                                                 :enhanced t)
                                             :output file-name)
                            ;:page-args (list :terminal (cl-ana.oplotting:wxt-term :size (cons 800 600)))
                            )))
