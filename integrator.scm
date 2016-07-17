;; Integrator
(define	(integrator init-val dx func-stream)
	(cons-stream	(* init-val dx)
			(add-streams	(integrator init-val dx func-stream)
					(map-stream 	(lambda (x) (* x dx))
							(force func-stream)))))
