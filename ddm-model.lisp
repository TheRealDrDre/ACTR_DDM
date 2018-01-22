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
(define-model ddm

(chunk-type response-mapping cue response)
(chunk-type task state)   

(add-dm (a isa chunk)
	(b isa chunk)
	(start isa chunk)
	(sr1 isa response-mapping cue a response left)
	(sr2 isa response-mapping cue b response right)
	(task-goal isa task state start)


)

(p retrieve-response
   =goal>
   isa task
   state start
   ?retrieval>
   buffer empty
   state free
