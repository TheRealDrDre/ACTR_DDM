;;; ==================================================================
;;; 2AFC SIMULATIONS CODE
;;; ==================================================================

(defun simulate (n &key (params nil) (report t) (utilities nil))
  "A generic function to run the model N times. Returns a table of performance measures with the params"
  (let ((results nil))
    (dotimes (i n)
      (let ((p (make-instance '2afc-task)))
	(suppress-warnings (reload))
	(2afc-reload p)
	(sgp :v nil
	     :style-warnings nil
	     :model-warnings nil)
	
	;; Applies the necessary parameters

	(when params
	  (sgp-fct (mapcan #'(lambda (x) (list (first x) (rest x))) params)))

	(run 3000)

	(let ((partial (calculate-choose-avoid
			(experiment-log (current-device)))))
	  (when utilities
	    (setf partial (append partial
				  (decision-utilities (decision-productions)))))
	  
	  (push partial
		results))))
    (if report
	;(mapcar 'float (list (apply 'mean (mapcar 'first results))
					;		     (apply 'mean (mapcar 'second results))))
	(list (mapcar #'float
		      (eval (let ((qres (mapcar #'(lambda (x) (list 'quote x)) results)))
			      `(mapcar 'mean ,@qres)))))

	(reverse results))))
