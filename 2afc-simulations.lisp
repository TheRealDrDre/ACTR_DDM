;;; ==================================================================
;;; 2AFC SIMULATIONS CODE
;;; ==================================================================

(defun simulate (n &key (params nil) (start 0))
  "A generic function to run the model N times. Returns a table of performance measures with the params"
  (let ((results nil))
    (dotimes (i n results)
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
	(let* ((formatted (extract-results p)))
	  (push (mapcar #'(lambda (x) (push (+ start i) x)) formatted)
		results))))))
