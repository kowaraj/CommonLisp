(defun mc-problem ()
	(format t "@mc-problem >~%")
	(defparameter *left-bank* '(m m m c c c))
	(defparameter *right-bank* '(x x x x x x))
	(defparameter *boat* '(x x))
	(defparameter *current-bank* *left-bank*)
	(defparameter *current* "left")
	(defparameter *fringe* ()) 
	(defparameter *init-state* '((m m m c c c) (x x) (x x x x x x)))
	
	;goal conditions
	;(defvar *goal-left-bank* '(x x x x x x))	
	(defvar *goal-left-bank* '(x m m x c c))
	(defvar *goal-boat-l* '(c m))
	(defvar *goal-boat-r* '(x x))
	
	;(defvar *goal-boat-l* '(x x))

	;list of all possible single transaction objects
	(setf *objects* '((m) (m m) (c) (c c) (c m)))
	;list of all possible transactions
	(setf *transactions* '(un-ld ld))

	(mc-search *init-state*)
)

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

(defun is-member? (mem lst)
	(if (eql lst nil)
		nil
		(if (equal mem (car lst))
			t
			(is-member? mem (cdr lst)))))
		

(defun set-location (parent-node)
	(setf *left-bank* (copy-list (nth 0 parent-node)))
	(setf *boat* (copy-list (nth 1 parent-node)))
	(setf *right-bank* (copy-list (nth 2 parent-node)))
	(if (is-on-left-bank?)
		(setf *current-bank* *left-bank*)
		(setf *current-bank* *right-bank*)))				

(defun do-transaction (n-of-tr parent-node)	
	;dbg(format t "@do-transaction > parent-node: ~A~%" parent-node)
	(setf fringe-tr '())
	(let ((tr (nth n-of-tr *transactions*)))
	;dbg(format t " transaction: ~A~%" tr)
	;	(break "5")
		(dotimes (K (length *objects*))
			(setf objs (nth K *objects*))
			(set-location parent-node)
			(setf new-node (get-node tr objs))
			;dbg(format t "new-node= ~A~%" new-node)
			(if (not (eql new-node 'nil))
				(setf fringe-tr (cons new-node fringe-tr)))))

	;dbg(format t " fringe-of-trans  = ~A~%" fringe-tr)
	;(break "6")
	fringe-tr)


;make all possible transactions, get a fringe
(defun make-fringe (parent-node)
	;(format t "boat is on the ~A bank: ~A~%" *current* parent-node)
	(setf fringe (list parent-node))
	(setf fringe-temp '())
	;dbg(format t " mf: fringe = ~A~%" fringe)

	(setf fringe-unload (do-transaction 0 parent-node)) ;unload
	;dbg(format t " mf: fringe-unload = ~A~%" fringe-unload)

	(if (not (equal *current* "left"))
		(is-resolved? fringe-unload))

	;(if (not (eql fringe-unload nil))
	(setf fringe-unload (append fringe fringe-unload))
	;dbg(format t " mf: fringe-unload = ~A~%" fringe-unload)

	(dotimes (N (length fringe-unload))
		;(break "2")
		;dbg(format t "^ [~A] fringe-load= ~A~%" N fringe-temp)
		(setf node (nth N fringe-unload))
		(setf fringe-temp (append (do-transaction 1 node) (copy-list fringe-temp)))) ;load
	
	;dbg(format t " mf: fringe-temp = ~A~%" fringe-temp)

	;exclude already existed nodes
	(dotimes (I (length fringe-temp))
		(setf node (nth I fringe-temp))
		(if (and (not (is-member? node fringe))
					(satisfy-rules? node))
			(setf fringe (cons node fringe))))

	;dbg(format t " fringe = ~A~%" fringe)
	fringe)

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
				

(defun is-resolved? (fringe)
	;if right bank contains 3M & 3C
	(dotimes (I (length fringe))
		;(format t "fringe-elem [~A] = ~A~%" I (nth I fringe))
		(setf test (nth 2 (nth I fringe)))
		;(format t "test= ~A~%" test)
		(multiple-value-bind (m c) (countmc test)
			;(format t "m= ~A, c= ~A~%" m c)	
			(if (and (equal m 3)
						(equal c 3))
				(progn					
					(mc-print-status)	
					(break "success!"))))))

(defun mc-search (parent-node)
;	(break "mc-search")
;	(format t "@mc-search >~%")
	(if (null parent-node)
		nil
		(let ((dummy 0))	
			;get a new fringe
			(mc-print-status)
			(setf fringe (make-fringe parent-node))
			;check if succeeded			
			;(check-fringe fringe)
			(is-resolved? fringe)
			;go deeper
			(mc-move)
			;(break "mc-move")
			(dotimes (I (length fringe))
				;CHECKME: check if the *left-bank* restored from stack
				(mc-search (nth I fringe))))))



(defun mc-add (lst obj)
;	(format t "@mc-add > lst=~A, obj=~A~%" lst obj)
	(if (null lst)
		nil
		(if (eq (car lst) 'x)
			(setf (car lst) obj)
			(mc-add (cdr lst) obj)))
;	(format t "result=~A~%" lst)
	lst)

(defun mc-sub (lst obj)
;	(format t "@mc-sub > lst=~A, obj=~A~%" lst obj)
	(if (eq lst nil)
		lst	
		(if (eq (car lst) obj)
			(setf (car lst) 'x)
			(mc-sub (cdr lst) obj)))
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


;```````````
; DISPLAY

(defun mc-print-status ()
	(if (equal *current* "left")
		(format t "~A+~A ......... ~A~%" *left-bank* *boat* *right-bank*)
		(format t "~A ......... ~A+~A~%" *left-bank* *boat* *right-bank*)))

