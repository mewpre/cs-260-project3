;; 	(load "lisp-unit.lisp")
;;	(use-package :lisp-unit)
;;	(load "aiproj3.ls 	p")
;;	(load "testingProject3.lsp")


;; needed for the unit testing
(in-package :cl-user)

;; preprocessor
;; generates all possible goal states
;; get-all-vars pulls in ?x, ?y, etc *InitPSD*
;; get-all-vals pulls in A, B, etc *GSpec*
(setq *GENERATED-GOALS* (remove-dup-ops (generate-all-goals *GSpec* (get-question-mark-things *GSpec*) (get-all-vars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: FIGURE OUT WHICH DFS FUNCTION NEEDS TO REMAIN

;; FROM GOOGLE DOC

;; Initializes the currdepth
(defun idddfs (root upperdepth)
	(idddfs-helper root 0 upperdepth))

;; Checks if upperdepth has been reached. If not, calls dfs with the currdepth. If that doesn’t work,
;; increases the currdepth by 1 and runs again.
(defun idddfs-helper (root currdepth upperdepth) 
	(cond
		((equal currdepth upperdepth) nil)
		((dfs root 1 currdepth nil) T)
		(T (idddfs-helper root (+ currdepth 1) upperdepth)))) 

;;DFS function
;;if depth > bound, return false
;;if node and goal are equivalent, return the path
;;else we do the search for the next depth for each potential action
;;all parameters are lists
;;path is list of lists
(defun dfs (node depth bound path)
  (cond ((> depth bound) nil)
	((goal-check node) (return path))
	(t (do ((sublist (getPotentialActions node) (cdr sublist)))
		 ((null sublist) nil)
			(print node) (print depth) (print sublist) (print (operate-on-state (car sublist) node))
	       (if (dfs (operate-on-state (car sublist) node) (+ 1 depth) bound (append path (car sublist)))
		   (return-from dfs true))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun op-valid-helper (currentState preconditions)
 (cond
  ((null preconditions) T)
  ((not (exists (car preconditions) currentState)) nil)
  (T (op-valid-helper currentState (cdr preconditions)))
 )
)


(defun op-valid (currentState op)
 (op-valid-helper currentState (cadr op))
)


(defun gpa-helper (currentState ops)
 (cond
  ((null ops) nil)
  ((op-valid currentState (car ops)) (append (list (caar ops)) (gpa-helper currentState (cdr ops))))
  (T (gpa-helper currentState (cdr ops))) 
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; getPotentialActions
;; Returns a list of potential actions that can be done given the current state
(defun getPotentialActions (currentState)
 (gpa-helper currentState (generate-all-the-things *OPS*))
)

;; basically a member test function
;; returns true if the element exists in list
(defun exists (ele mlist) 	
	(cond
		((null mlist) nil)
		((equal ele (car mlist)) t)
		(t (exists ele (cdr mlist)))
	)
)

;; return the index of an element in a list
(defun index (ele list)
	(index-helper ele list 0)
)

;; helper function for index()
(defun index-helper (ele list num) 
	(cond 
		((null list) nil)
		((equal ele (car list)) num)
		(T (index-helper ele (cdr list) (+ num 1)))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uses 'replacer' to replace 'replacee' in a given list
;; returns the list
(defun mreplace (replacee replacer list)
	(setf (nth (index replacee list) list) replacer) 
	list
)

;; recursive replace function
(defun rr2 (list replacee replacer)
 (cond
  ((null list) '())
  ((listp (car list)) (cons (rr2 (car list) replacee replacer) (rr2 (cdr list) replacee replacer)))
  ((equal (car list) replacee) (cons replacer (rr2 (cdr list) replacee replacer)))
  (T (cons (car list) (rr2 (cdr list) replacee replacer)))
 )
)

;; returns a list of all variables in the action defined in *OPS* 
(defun get-op-variables (op)
	(cdr (car op))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-all-vars
;; takes in the world's preconditional state
;; outputs a list of all varibles in the world
(defun get-all-vars ()
	(remove-duplicates (flatten (get-all-vars-helper *InitPSD*)))
)

;; helper function for get-all-vars
(defun get-all-vars-helper (state)
	(cond
		((null state) nil)
		((null (cdar state)) (get-all-vars-helper (cdr state)))
		(T (cons (cdar state) (get-all-vars-helper (cdr state))))
	)
)

;; completely flattens a list, regardless of degree of nesting
(defun flatten (mylist)
	(cond
		((null mylist) nil)
		((atom (car mylist)) (cons (car mylist) (flatten (cdr mylist))))
		((listp (car mylist)) (append (flatten (car mylist)) (flatten (cdr mylist))))
		(T nil)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; top-level
(defun generate-all-ops (ops values)
 (cond
  ((null ops) '())
  (T (remove-dup-ops (append (append (generate-all-op-versions (car ops) values) 
			(generate-all-ops (cdr ops) values))
			(append (generate-all-op-versions (car ops) (reverse values)) 
			(generate-all-ops (cdr ops) (reverse values)))
					)
		)
   )
)
)

(defun op-version-helper (op var val)
 (cond
  ((null var) op)
  (T (op-version-helper (rr2 op (car var) (car val)) (cdr var) (cdr val)))
 )
)

(defun generate-pair-op-versions (op values)
 (cond
  ((null values) '())
  (T (append (list (op-version-helper op (cdr (car op)) (car values))) (generate-pair-op-versions op (cdr values))))
 )
)

(defun generate-single-op-versions (op values)
 (cond
  ((null values) '())
  (T (append (list (op-version-helper op (cdr (car op)) (list (car values)))) (generate-single-op-versions op (cdr values))))
 )
)

(defun generate-all-op-versions (op values)
 (cond
  ((null values) '())
  ((equal (length (cdr (car op))) 2) (generate-pair-op-versions op (generate-pairs values)))
  ((equal (length (cdr (car op))) 1) (generate-single-op-versions op values))
 )
)

(defun gp-helper (first losecond)
 (cond
  ((null losecond) '())
  (T (cons (list first (car losecond)) (gp-helper first (cdr losecond))))
 )
)
(defun generate-pairs (list)
 (cond
  ((null list) '())
  (T (append (gp-helper (car list) (cdr list)) (generate-pairs (cdr list))))
)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GENERATE ALL THE THINGS!!!  or all the possible actions
(defun generate-all-the-things (ops)
	(generate-all-ops ops (get-all-vars))
)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; update state function

;; remove sub-list
(defun my-remove (e l)
	(cond 
		((null l) '())
		((equal e (car l)) (my-remove e (cdr l)))
		(T (cons (car l) (my-remove e (cdr l))))
	)
)

;; remove all
(defun remove-all (es l)
	(cond
		((null es) l)
		(T (remove-all (cdr es) (my-remove (car es) l)))
	)
)

;; removing operations and then adding the new one
(defun operate-on-state (op state)
	(remove-dup-ops (append (remove-all (third op) state) (fourth op)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove duplicates
;; removes all duplicates, even sublists
;; if (car mylist) is a list, then we need to recurse through each list element
;;		of mylist and compare inside elements
(defun remove-dup-ops (mylist)
	(cond
		((null mylist) nil)
		((and (atom (car mylist)) (member (car mylist) mylist)) (remove-dup-ops (cdr mylist)))
		((listp (car mylist))  (cons (car mylist) (remove-dup-ops (remove-dup-ops-helper (car mylist) (cdr mylist)))))
		(T (cons (car mylist) (remove-dup-ops (cdr mylist))))
	)
)

;; remove-dup-ops-helper
(defun remove-dup-ops-helper (front mylist)
	(cond
		((null mylist) nil)
		((equal front (car mylist)) (remove-dup-ops-helper front (cdr mylist)))
		(T (cons (car mylist) (remove-dup-ops-helper front (cdr mylist))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find ?x vars
;; finds all variables WITH question marks
(defun get-question-mark-things (goal)
	(cond
		((null goal) nil)
		((null (get-question-mark-helper (car goal))) (get-question-mark-things (cdr goal)))
		(T (flatten (cons (get-question-mark-helper (car goal)) (get-question-mark-things (cdr goal)))))
	)
)

;; find ?x helper
(defun get-question-mark-helper (goal)
	(cond
		((null goal) nil)
		((equal (char (symbol-name (car goal)) 0) #\?) (cons (car goal) (get-question-mark-helper (cdr goal))))
		(T (get-question-mark-helper (cdr goal)))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; combination
(defun combination (k list)
  "Generate the combinations of K distinct objects chosen from the N elements of a list."
  (cond
    ((zerop k)   '(()))  ; a single combination of 0 element in any list.
    ((endp list) '())    ; no combination in an empty list.
    (t (append (mapcar (let ((neo (first list)))
                         (lambda (subcomb) (cons neo subcomb))) ; combs with neo
                       (combination (1- k) (rest list)))
               (combination k (rest list)))))) ; combs without neo


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-let-assignments (vars vals)
 (cond
  ((null vars) '())
  (T (append (list (list (car vars) (car vals))) (generate-let-assignments (cdr vars) (cdr vals))))
 )
)

(defun generate-replace-pairs (vals vars)
 (loop for i in (combination (length vars) vals) collect (generate-let-assignments vars i))
)

(defun replace-sets (list pair-list)
 (cond
  ((null pair-list) list)
  (T (replace-sets (rr2 list (first (car pair-list)) (second (car pair-list))) (cdr pair-list)))
 )
)

;; vars is 
(defun generate-all-goals (goal vars vals)
	(append (loop for i in (generate-replace-pairs vals vars) collect (replace-sets goal i))
			(loop for i in (generate-replace-pairs (reverse vals) vars) collect (replace-sets goal i))
	)
)

(defun goal-check-helper (current-state goal-list)
 (cond
  ((null goal-list) nil)
  ((checkStateEquivalence current-state (car goal-list)) T)
  (T (goal-check-helper current-state (cdr goal-list)))
 )
)

(defun goal-check (current-state)
 (goal-check-helper current-state *GENERATED-GOALS*)
)



