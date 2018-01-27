;;; ==================================================================
;;; 2AFC SIMULATIONS CODE
;;; ==================================================================

(defun simulate (n &key (params nil) (start 0))
  "A generic function to run the model N times. Returns a table of performance measures with the params"
  (let ((results nil)
	(colnames (append (list "idx")
			  (mapcar #'(lambda (x)
				      (string-downcase
				       (format nil "~A" x)))
				  (remove-if #'numberp params))
			  (list "stim" "response" "rt"))))
					    
    (dotimes (i n (append (list colnames) (reverse results)))
      (let ((p (make-instance '2afc-task)))
	(suppress-warnings (reload))
	(2afc-reload p)
	(sgp :v nil
	     :style-warnings nil
	     :model-warnings nil)
	
	;; Applies the necessary parameters

	(when params
	  (sgp-fct params))
	(run 3000)
	(let* ((formatted (extract-results p))
	       (information (cons (+ start i)
				  (remove-if-not #'numberp params))))
	  (dolist (trial formatted)
	    (push (append information trial)
		  results)))))))

(defun write-csv (table filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :overwrite
		       :if-does-not-exist :create)
    (dolist (row table)
      (format out "~{~a~^,~}~%" row))))


