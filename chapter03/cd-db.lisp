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
  (loop (add-record (prompt-for-cd))
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
