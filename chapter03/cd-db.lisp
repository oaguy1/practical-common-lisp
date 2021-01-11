;;;; Cd Database from Chapter 3 of PCL

;; Global Variables for this file
(defvar *db* nil)

;; Function to create a CD record for our DB
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; Function to file a CD
(defun add-record (cd) (push cd *db*))

;; Function to dump to STDOUT using insame FORMAT function
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;; Prompt for input
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; Nice mini UI for inputting CDs
(defun prompt-for-cds ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

;; Add to DB until asked to stop
(defun add-cds ()
  (loop (add-record (prompt-for-cds))
	(if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; Save the DB to a file
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; Load the DB from a file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; Select from DB by artists (naive)
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

;; More generalized select function
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; Naive selector-fn generator that is *vaguely* SQL like...
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; Naive update function
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db* )))

;; Function to delete rows selected by selector-fn
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; baby's first macro
(defmacro backwards (expr) (reverse expr))

;; build comparison for field
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

;; build comparison list
(defun make-comparison-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

;; where defined as a macro
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))
