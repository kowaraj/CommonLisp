;``````````````````````````````````````````````````````````
; 	Canibals' problem. 
;	
;	v.2
;	apash, 2010.01.29
;
;``````````````````````````````````````````````````````````


;;; MAIN ``````````````````````````````````````````````````

(defun mc-problem ()
	;(format t "@mc-problem >~%")
	(defparameter *left-bank* '(m m m c c c))
	(defparameter *right-bank* '(x x x x x x))
	(defparameter *boat* '(x x))
	(defparameter *current-bank* *left-bank*)
	(defparameter *current* "left")
	(defparameter *fringe* ()) 
	(defparameter *fringe-tested* '()) 
	(defparameter *init-state* '((m m m c c c) (x x) (x x x x x x)))
	(defparameter *tabn* 0)
	(defconstant *m-num-key* 3)
	(defconstant *c-num-key* 3)

	;goal conditions
	;(defvar *goal-left-bank* '(x x x x x x))	
	(defvar *goal-left-bank* '(x m m x c c))
	(defvar *goal-boat-l* '(c m))
	(defvar *goal-boat-r* '(x x))
	
	;(defvar *goal-boat-l* '(x x))

	;list of all possible single transaction objects
	(setf *objects* '((m) (m m) (c) (c c) (c m) ()))
	;list of all possible transactions
	(setf *transactions* '(un-ld ld))

	(setf *fringe-tested* '())
	(mc-search *init-state*)
)

;;; SEARCH `````````````````````````````````````````````````

(defun mc-search (parent-node)
	(setf *tabn* (+ 1 *tabn*))
	(format t "~%>                   [~A]~%" *tabn*)

	(if (null parent-node)
		nil
		(let ((dummy 0))	
			;get a new fringe
			(format t "current status: ") (mc-print-status)
			(format t "search for parent-node: ~A~%" parent-node)
			(setf fringe (make-fringe parent-node))
			(format t "parent-node's fringe: ~A~%" fringe)
			(setf *fringe-tested* (cons parent-node *fringe-tested*))
;			(format t "already tested fringe = ~A~%" *fringe-tested*)
			(setf fringe (exclude-tested-nodes fringe *fringe-tested*))
			(format t "cleaned fringe: ~A~%" fringe)
			
			;go deeper
			(mc-move)
			(dotimes (I (length fringe))
				(format t "taking the node #~A: ~A~%" I (nth I fringe))
				(set-location (nth I fringe))
				(break "...")
				(mc-search (nth I fringe)))))
	(setf *tabn* (- 1 *tabn*))
)


;;; FRINGE ````````````````````````````````````````````````

;make all possible transactions, get a fringe
(defun make-fringe (parent-node)
	(setf fringe (list parent-node))
	(setf fringe-temp '())

	(format t "make a fringe...~%")
	(setf fringe-unload (do-transaction 0 parent-node)) ;unload
	(if (not (equal *current* "left"))
		(is-resolved? fringe-unload))
	(setf fringe-unload (append fringe fringe-unload))
	(dotimes (N (length fringe-unload))
		(setf node (nth N fringe-unload))
		(setf fringe-temp (append (do-transaction 1 node) (copy-list fringe-temp)))) ;load
	
	(is-resolved? fringe-temp) ;must be above "the refining"
	(format t "done, fringe: ~A~%" fringe-temp)
	(format t "refine the fringe...~%")
	(setf fringe (refine-fringe fringe-temp))
	fringe)

;;; TEST `````````````````````````````````````````````````````	

(defun is-resolved? (fringe)
	;if right bank contains 3M & 3C
	(dotimes (I (length fringe))
		(setf test (nth 2 (nth I fringe)))
		(multiple-value-bind (m c) (countmc test)
			(if (and (equal m *m-num-key*)
						(equal c *c-num-key*))
				(progn					
					(format t "~%salvation: ")
					(mc-print-status)	
					(break "success!"))))))

;;; DISPLAY ````````````````````````````````````````````````` 

(defun mc-print-status ()
	(if (equal *current* "left")
		(format t "~A ~A            ~A~%" *left-bank* *boat* *right-bank*)
		(format t "~A            ~A ~A~%" *left-bank* *boat* *right-bank*)))


;;; `````````````````````````````````````````````````````````

(defun do-transaction (n-of-tr parent-node)	
	(setf fringe-tr '())
	(let ((tr (nth n-of-tr *transactions*)))
		;dbg(format t " @tr: ~A for ~A~%" tr parent-node)
		(dotimes (K (length *objects*))
			(setf objs (nth K *objects*))
			(set-location parent-node)
			;dbg(format t " @objs: ~A~%" objs)
			(setf new-node (get-node tr objs))
			;dbg(format t "new-node= ~A~%" new-node)
			(if (not (eql new-node 'nil))
				(setf fringe-tr (cons new-node fringe-tr)))))

	;dbg(format t " @fringe of trans: ~A~%" fringe-tr)
	fringe-tr)

;check the boat
(defun boat-empty-enough? (objs)
	;(format t "len= ~A~% boat=~A~%" (length objs) *boat*)
	;(format t "x= ~A~%" (countx *boat*))
	(if (<= (length objs) (countx *boat*))
		t
		nil))

;check the bank for 'm' & 'c'
(defun bank-contains-enough? (objs)
	(multiple-value-bind (m c) (countmc objs)
		(multiple-value-bind (m-bank c-bank) (countmc *current-bank*)
			(if (and (>= m-bank m)
						(>= c-bank c))
				t
				nil))))

;check the boat for 'm' & 'c'
(defun boat-contains-enough? (objs)
	(multiple-value-bind (m c) (countmc objs)
		(multiple-value-bind (m-boat c-boat) (countmc *boat*)
			(if (and (>= m-boat m)
						(>= c-boat c))
				t
				nil))))

;check if transaction 'LOAD' is possible
(defun load-possible? (objs)
;	(format t "@load-possible? > objs= ~A~%" objs)
	(if (and (boat-empty-enough? objs)
				(bank-contains-enough? objs))
		t
		nil))

;check if transaction 'UNLOAD' is possible
(defun unload-possible? (objs)
;	(format t "@unload-possible? > objs= ~A~%" objs)
	(boat-contains-enough? objs))

;check if there is anyone in the boat
(defun move-possible? ()
	(if (< (countx *boat*) 2)
		t
		nil))

(defun satisfy-rules-for-bank? (lst)
	(multiple-value-bind (m c) (countmc lst)
		(if (or (equal m c)
				  (or (equal m 0)
						(equal c 0)))
			t
			nil)))

(defun satisfy-rules-for-boat? (lst)
	(multiple-value-bind (m c) (countmc lst)
		(if (and (or (equal m c)
				  		 (or (equal m 0)
							  (equal c 0)))
					(> (+ m c) 0))	
			t
			nil)))

(defun satisfy-rules? (new-node)
	(if (and (satisfy-rules-for-bank? (nth 0 new-node))
				(satisfy-rules-for-bank? (nth 2 new-node))
				(satisfy-rules-for-boat? (nth 1 new-node)))
		t
		nil))

			
(defun refine-fringe (f)
	(setf f-out '())
	(dotimes (I (length f))
		(setf node (nth I f))
		(if (and (not (is-member? node f-out))
					(satisfy-rules? node))
			(setf f-out (cons node f-out))))
;	(format t "refine-fringe= ~A~%" f-out)
	f-out)

(defun exclude-tested-nodes (f f-tested)
	(setf f-out '())
	(dotimes (I (length f))
		(setf node (nth I f))
		(if (not (is-member? node f-tested))
				(setf f-out (cons node f-out))))
;	(format t "excluded-fringe= ~A~%" f-out)
	f-out)

(defun is-fringe-valid? (fringe)
	(dotimes (I (length fringe))
	(setf node (nth I fringe))
		(if (satisfy-rules? node)
			t
			nil)))

(defun mc-load (objs)
	(dotimes (N (length objs))
		(setf obj (nth N objs))
 		(mc-sub *current-bank* obj)
  		(mc-add *boat* obj)))

(defun mc-unload (objs)
	(dotimes (N (length objs))
		(setf obj (nth N objs))
	 	(mc-add *current-bank* obj)
	 	(mc-sub *boat* obj)))

;make a complete transaction, get a new node
(defun get-node (tr objs)
	(if (eql tr 'ld)
		(progn 
	 		(if (load-possible? objs)
				(mc-load objs)
				(return-from get-node nil)))
 		(progn
			(if (unload-possible? objs)
				(mc-unload objs)
				(return-from get-node nil))))

	;new node after a transaction
	(list *left-bank* *boat* *right-bank*))

(defun equal-node (n1 n2)
	(if (and	
				(multiple-value-bind (m1 c1) (countmc (nth 0 n1))
					(multiple-value-bind (m2 c2) (countmc (nth 0 n2))
						(if (and (equal m1 m2)
									(equal c1 c2))
								t
								nil)))
				(multiple-value-bind (m1 c1) (countmc (nth 1 n1))
					(multiple-value-bind (m2 c2) (countmc (nth 1 n2))
						(if (and (equal m1 m2)
									(equal c1 c2))
								t
								nil)))
				(multiple-value-bind (m1 c1) (countmc (nth 3 n1))
					(multiple-value-bind (m2 c2) (countmc (nth 3 n2))
						(if (and (equal m1 m2)
									(equal c1 c2))
								t
								nil))))
			t
			nil))
				
		

(defun is-member? (mem lst)
	(if (eql lst nil)
		nil
		(if (equal-node mem (car lst))
			t
			(is-member? mem (cdr lst)))))
		

(defun set-location (parent-node)
	(setf *left-bank* (copy-list (nth 0 parent-node)))
	(setf *boat* (copy-list (nth 1 parent-node)))
	(setf *right-bank* (copy-list (nth 2 parent-node)))
	(if (is-on-left-bank?)
		(setf *current-bank* *left-bank*)
		(setf *current-bank* *right-bank*)))				


(defun countx (lst)
	(if (atom lst)
		0
		(let ((count-x 0))
			(dotimes (N (length lst))
				(if (equal 'x (nth N lst))
					(setf count-x (+ count-x 1))))
			count-x)))

(defun countmc (lst)
	(if (atom lst)
		(values 0 0)
		(progn
			(setf count-m 0)
			(setf count-c 0)
			(dotimes (N (length lst))
				(setf s (nth N lst))
				(if (equal s 'm)
					(setf count-m (+ count-m 1)))
				(if (equal s 'c)
					(setf count-c (+ count-c 1)))

			)
			;(format t "m,c == ~A, ~A~%" count-m count-c)
			(values count-m count-c))))
				

(defun mc-add (lst obj)
;	(format t "@mc-add > lst=~A, obj=~A~%" lst obj)
	(if (null lst)
		nil
		(if (eq obj nil)
			lst
			(if (eq (car lst) 'x)
				(setf (car lst) obj)
				(mc-add (cdr lst) obj))))
;	(format t "result=~A~%" lst)
	lst)

(defun mc-sub (lst obj)
;	(format t "@mc-sub > lst=~A, obj=~A~%" lst obj)
	(if (eq lst nil)
		lst
		(if (eq obj nil)
			lst
			(if (eq (car lst) obj)
				(setf (car lst) 'x)
				(mc-sub (cdr lst) obj))))
;	(format t "result=~A~%" lst)
	lst)

;````````````
; MOVE

(defun is-on-left-bank? ()
	(if (equal *current* "left")
		t
		nil))

(defun move->right ()
;	(format t "@move->right~%")
	(setf *current* "right"))

(defun move->left ()
;	(format t "@move->left~%")
	(setf *current* "left"))

(defun mc-move ()
	(if (is-on-left-bank?)
		(move->right)
		(move->left)))


