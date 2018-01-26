;;; ------------------------------------------------------------------
;;; 2AFC TASK IN ACT-R and DDM
;;; ==================================================================
;;; This is the code to implement an abstract and general version of
;;; a two-alternative forced-hoice task (2AFC) in ACT-R. The goal is
;;; to examine the model's behavior across a limited set of
;;; parameters that can be compared against Ratcliff's Drfit-Diffusion
;;; model, a standard model to interpret response times and latencies.
;;; ==================================================================
;;;
;;; 2AFC in DDM
;;; ------
;;; In DDM, decisions are modeled as drifting processes that proceed
;;; towards one of two possible thresholds (corresponding to the
;;; two options). The model is controlled by three parameters:
;;;   (1) a drift parameter 'v'
;;;   (2) a threshold or boundary parameter 'a', which represents
;;;       the distance from either of the response thresholds.
;;;   (c) a non-decision time 'T', which represents response and
;;;       encoding times.
;;;
;;; 2AFC in ACT-R
;;; -------------
;;; In ACT-R, decisions can be made through several mechanisms,
;;; procedural and declarative. Here, we describe a general enough
;;; mechanisms that comprises both, and is based on the following
;;; steps:
;;;   (a) The model holds in mind an internal correctness criterion
;;;       in the working memory (WM) buffer, and maintains the
;;;       two options in the visual buffer.
;;;   (b) The model retrieves the S/R mappings from LTM.
;;;   (c) The model proceeds through a check stage, during which
;;;       it compares the retrieved S/R against the internal
;;;       correctness criterion.
;;;       (c1) If a match is found, the model responds.
;;;       (c2) If not, the model either...
;;;            (c.2.1) restarts the search with probability 'p'
;;;            (c.2.2) responds anyway with probability (1 - p).
;;;
;;; ACT-R Parameters
;;; ----------------
;;; The main parameters in the ACT-R implementation are:
;;;   (a) W: the spreading activation from the internal correctness
;;;       criterion. This corresponds to WM in ACT-R and should
;;;       correspond to 'v' in DDM.
;;;   (b) S: The noise in declarative memory. This mimics the
;;;       resistance to spreading activation.
;;;   (c) U: The perceived utility of restarting the search process,
;;;       defined in terms of RL expected value V. This determines
;;;       the boundary parameter 'a'.
;;; ==================================================================

(clear-all)

(define-model 2afc

(sgp :esc t
     :auto-attend t
     :er t
     :mas 1.69  ;; This is 1 + log(2), so that S - log(Sji) = 1. 
     :bll nil
     :imaginal-activation 1.0
     :visual-activation 1.0
     :ga 0.0
     :blc 1.0
     :ans 0.1)

(chunk-type 2afc-object; (:include visual-object))
	    kind value)

(chunk-type (2afc-location (:include visual-location))
	    kind nature)

(chunk-type response-mapping kind cue response)

(chunk-type task state)

(chunk-type criterion criterion)

(add-dm (2afc-screen isa chunk)
	(2afc-location isa chunk)
	(2afc-stimulus isa chunk)
	(pause isa chunk)
	(done isa chunk)

	;; Stimuli
        (correct isa chunk)
	(incorrect isa chunk)
	
	;; States
	(start isa chunk)
	(check isa chunk)
	(response isa chunk)
	(response-mapping isa chunk)
	
	;; Task rules
	(sr1 isa response-mapping
	     kind response-mapping
	     cue correct
	     response left)
	(sr2 isa response-mapping
	     kind response-mapping
	     cue incorrect
	     response right)
	(2afc isa task
	      state start)
	(criterion isa criterion 
	      criterion correct))

;;; ------------------------------------------------------------------
;;; ENCODING
;;; ------------------------------------------------------------------
;;; This production needs to fire only once; the model will
;;; automatically re-encode the same location when the screen is
;;; updated.
;;; ------------------------------------------------------------------

(p encode
   =goal>
     isa task
     state start
   ?visual>
     buffer empty
     state free
==>
   +visual-location>
     kind 2afc-location
)
;;; ------------------------------------------------------------------
;;; RESPONSE PHASE
;;; ------------------------------------------------------------------

(p retrieve-response
   =goal>
     isa task
     state start
   =visual>
     isa 2afc-object
     kind 2afc-stimulus
   ?retrieval>
     buffer empty
     state free
   ?manual>
     preparation free
     processor free
     execution free
==>
   =visual>
   =goal>
     state check
   +retrieval>
     kind response-mapping   
)

;;; ------------------------------------------------------------------
;;; CHECK PHASE
;;; ------------------------------------------------------------------
;;; General algorithm
;;;
;;;              (Correct?)
;;;              /        \
;;;            Yes        No
;;;            /           \
;;;        Respond      +Conflict+
;;;                        / \
;;;                       p  1-p
;;;                      /     \
;;;                  Restart  Respond
;;;
;;; ------------------------------------------------------------------

(p proceed-correct
   =goal>
     isa task
     state check
   =imaginal>
     criterion =C
   =visual>
     isa 2afc-object
     kind 2afc-stimulus
   ?retrieval>
     buffer full
     state free
   =retrieval>
     kind response-mapping
     cue =C
==>
   =goal>
     state response
   =retrieval>
   =visual>
   =imaginal>
)

(p proceed-incorrect
   =goal>
     isa task
     state check
   =imaginal>
     criterion =C
   =visual>
     isa 2afc-object
     kind 2afc-stimulus
   ?retrieval>
     buffer full
     state free
   =retrieval>
     kind response-mapping
   - cue =C
==>
   =goal>
     state response
   =retrieval>
   =visual>
   =imaginal>
   )

(p restart
   =goal>
     isa task
     state check
   =imaginal>
     criterion =C
   =visual>
     isa 2afc-object
     kind 2afc-stimulus
   ?retrieval>
     buffer full
     state free
   =retrieval>
     kind response-mapping
   - cue =C
==>
   =goal>
     state start
   -retrieval>
   =visual>
   =imaginal>
)

;;; ------------------------------------------------------------------
;;; RESPONSE
;;; ------------------------------------------------------------------

(p respond
   =goal>
     isa task
     state response
   ?retrieval>
     buffer full
     state free
   =retrieval>
     kind response-mapping
     response =HAND
   ?manual>
     preparation free
     processor free
     execution free
==>
   +manual>
     isa punch
     finger index
     hand =HAND
   =goal>
     state start  
)

(goal-focus 2afc)

(set-buffer-chunk 'imaginal 'criterion) 

) ; End of model
