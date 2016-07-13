;;; Enumerate Stream
(define (enumerate-stream init final inc)
	(if 	(> init final)
		'()
		(cons-stream	init
				(enumerate-stream 	(inc init)
							final
							inc))))

;;; Map Stream
(define (map-stream proc s)
	(if	(stream-null? s)
		'()
		(cons-stream	(proc (stream-car s))
				(map-stream 	proc	
						(stream-cdr s)))))

;;; Filter Stream
(define (filter-stream pred s)
	(if 	(stream-null? s)
		'()
		(if	(pred (stream-car s))
			(cons-stream	(stream-car s)
					(filter-stream pred (stream-cdr s)))
			(filter-stream pred (stream-cdr s)))))

;;; Accumulate
(define (accumulate proc init-val s)
	(if	(stream-null? s)
		init-val
		(accumulate	proc
				(proc (stream-car s) init-val)
				(stream-cdr s))))
;;; Append Stream
(define (append-stream s1 s2)
	(if	(stream-null? s1)
		s2
		(cons-stream	(stream-car s1)
				(append-stream	(stream-cdr s1)
						s2))))

;;; Flatten Stream
(define (flatten-stream st-o-st)
	(if	(stream-null? st-o-st)
		'()
		(append-stream	(stream-car st-o-st)
				(flatten-stream (stream-cdr st-o-st)))))
				
;;; Print Entire Stream
(define (print-stream s)
	(cond	((stream-null? s) "Done")
		(else	(write (stream-car s))
			(write-char #\tab)
			(print-stream (stream-cdr s)))))


;;; Print Stream n
(define (print-stream-n n s)
	(if	(= n 0)
		"Done"
		((lambda ()
			(write (stream-car s))
			(write-char #\tab)
			(print-stream-n (-1+ n) (stream-cdr s))))))
;;; Add streams
;;; Add 2 streams elements by elements
(define (add-streams s1 s2)
	(cond	((stream-null? s1)	s2)
		((stream-null? s2)	s1)
		(else	(cons-stream	(+ (stream-car  s1) (stream-car s2))
					(add-streams (stream-cdr s1) (stream-cdr s2))))))
