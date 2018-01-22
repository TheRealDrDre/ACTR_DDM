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
