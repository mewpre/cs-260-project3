;; testing code for project 3

;; For this testing code, we am using LispUnit. I (Amber) recieved this from 
;;	Dr. Chris Reisbeck from Northwestern University. In the submission
;; 	file, I have include the necessary files for running this testing
;;	code.
;; source: http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html


(in-package :lisp-unit)
(in-package :cl-user)

(setf *OPS*
	'(((PICKUP ?x) 
	 ((ONTABLE ?x) (CLEAR ?x) (HANDEMPTY))	;; PRE list
	 ((ONTABLE ?x) (CLEAR ?x) (HANDEMPTY))	;; DEL list
	 ((HOLDING ?x))							;; ADD list
	)
	((PUTDOWN ?x)
	 ((HOLDING ?x))							;; PRE list
	 ((HOLDING ?x))							;; DEL list
	 ((ONTABLE ?x) (CLEAR ?x) (HANDEMPTY))	;; ADD list
	)
	((STACK ?x ?y)
	 ((HOLDING ?x) (CLEAR ?y))				;; PRE list
	 ((HOLDING ?x) (CLEAR ?y))				;; DEL list
	 ((HANDEMPTY) (ON ?x ?y) (CLEAR ?x))	;; ADD list
	)
	((UNSTACK ?x ?y)
	 ((HANDEMPTY) (ON ?x ?y) (CLEAR ?x))	;; PRE list
	 ((HANDEMPTY) (ON ?x ?y) (CLEAR ?x))	;; DEL list
	 ((CLEAR ?x) (HOLDING ?y))				;; ADD list
	)
	)
)

(setf *InitPSD*
	'((CLEAR B) (CLEAR C) (ON C A) (ONTABLE A) (ONTABLE B) (HANDEMPTY))
)

(setf *GSpec*
	'((ON C B) (ON A C))
)

;; test operate-on-state
;; operate-on-state (op state)
(define-test test-op-on-state1
	(assert-equal '((HOLDING A)) (operate-on-state '((PUTDOWN A)) '((HOLDING A))))
)

(define-test test-op-on-state2
	(assert-equal '((HANDEMPTY) (ON A B) (CLEAR A)) 
		(operate-on-state '((STACK A B)
							((HOLDING A) (CLEAR B))	;; pre
							((HOLDING A) (CLEAR B))	;; delete
							((HANDEMPTY) (ON A B) (CLEAR A))	;; add
							)
		'((HOLDING A) (CLEAR B))))
)

;; test getPotentialActions
;;		getPotentialActions(currentState)
;;		currentState is a list
(define-test testPA1
	(assert-equal '((PICKUP B) (UNSTACK C A))
		(getPotentialActions '((CLEAR C) (CLEAR B) (ON C A) (ONTABLE A) (ONTABLE B) (HANDEMPTY)))
	)
)
;; FAILING - returning just (PUTDOWN B)
(define-test testPA2
	(assert-equal '((PUTDOWN B) (STACK B C))
		(getPotentialActions '((CLEAR C) (ON C A) (HOLDING B) (ONTABLE A))))
)
(define-test testPA3
	(assert-equal '((PUTDOWN A)) (getPotentialActions '((HOLDING A))))
)
(define-test testPA4
	(assert-equal '((PICKUP A)) (getPotentialActions '((ONTABLE A) (CLEAR A) (HANDEMPTY))))
)
(define-test testPA5
	(assert-equal '() (getPotentialActions '(())))
)

(define-test testPA6
	(assert-equal '((PUTDOWN A) (STACK A B)) (getPotentialActions '((HOLDING A) (CLEAR B))))
)














