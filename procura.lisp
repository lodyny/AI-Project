;; Code about the searching algorithms
;; Developed by Cesar Nero and David Afonso
;; Artificial Intelligence - IPS 2018/2019


;; Searching Algorithms
(defun BFS (open &optional (closed '()))
"Breath-First Search"
  (let 
    ((expand-size (length open)))
    (cond ((= expand-size 0) nil)
    (T (let* ((current-node (first open))
              (expanded-list (expand-node current-node)))
        (if (equal (is-board-empty (first current-node)) T)
          (list (get-solution-path current-node) (length open) (length closed))
          (BFS  (concatenate 'list (rest open) (remove-nil (remove-duplicated-nodes expanded-list open closed))) (concatenate 'list closed (list current-node)))))))))

(defun DFS (open max-depth &optional (closed '()))
"Depth-First Search"
   (cond ((= (length open) 0) nil)
    ((> (length (get-solution-path (first open))) max-depth) (DFS (rest open) max-depth (concatenate 'list closed (first open))))
    (T (let* ((current-node (first open))(expanded-list (expand-node current-node)))
      (if (equal (is-board-empty (get-current-node current-node)) T)
        (list (get-solution-path current-node) (length open) (- (length (get-solution-path (first open))) 1 ))
          (DFS  (concatenate 'list expanded-list (rest open)) max-depth (concatenate 'list closed (list current-node))))))))


(defun A*(expandFunction hFunction open &optional (closed '()) (expanded-nodes 0))
"A-STAR Algorithm"
  (cond ((= (length open) 0) nil)
   (T (let* ((chosen-node (get-lowest-f open))
            (expanded-list (funcall expandFunction chosen-node hFunction))
            (recalculated-closed (recalculate-closed expanded-list closed chosen-node))
            (recalculated-open (recalculate-open expanded-list (rest open) chosen-node))
            (new-open (remove-nil (append recalculated-open (remove-duplicate-values expanded-list recalculated-open) recalculated-closed))))
        (if (=  (length expanded-list) 0)
          (list (get-solution-path chosen-node) (length open) (length closed) expanded-nodes)
          (A* expandFunction hFunction new-open (remove-duplicate-values (concatenate 'list closed (list chosen-node)) recalculated-closed) (1+ expanded-nodes)))))))

(defun IDA*(expandFunction hFunction open &optional (closed '()) (limiar (get-node-f (first open))) (temp-open-nodes-length 0) (expanded-nodes 0))
"Iterative Deepening A-START Algorithm"
  (cond
   ((= (length open) 0) nil)
   (T
     (let ((chosen-node (get-lowest-f (get-max-value-nodes open limiar))))
       (if (null chosen-node)
           (IDA* expandFunction hFunction open closed (get-minf-value open) (+ temp-open-nodes-length (length open) (length closed)))
         (let*
            ((expanded-list (funcall expandFunction chosen-node hFunction))
            (recalculated-closed (recalculate-closed expanded-list closed chosen-node))
            (recalculated-open (recalculate-open expanded-list (rest open) chosen-node))
            (new-open (remove-nil (append recalculated-open (remove-duplicate-values expanded-list recalculated-open) recalculated-closed))))
      (if (=  (length expanded-list) 0)
       (list (get-solution-path chosen-node) (length open) (length closed) expanded-nodes temp-open-nodes-length)     
       (IDA* expandFunction hFunction new-open (remove-duplicate-values (concatenate 'list closed (list chosen-node)) recalculated-closed) limiar temp-open-nodes-length (1+ expanded-nodes)))))))))

;; Node Stuff
;; Node ::= (<state> <parent> <g> <h> <pieces-left>)
(defun construct-node (board parent pieces &optional (g 0) (h 0))
"Build a node"
  (list board parent g h pieces)
)


(defun remove-duplicated-nodes(list open closed)
"Remove from the list the duplicated nodes"
  (remove-duplicate-values (remove-duplicate-values list open) closed)                
)

(defun remove-duplicate-values (list1 list2)
"Remove from the list duplicated values"
  (if (or (null list1) (null list2))
      list1
      (remove-nil (mapcar #'(lambda(x) (if (eval (cons 'or (mapcar #'(lambda(n) (equal (first x) (first n))) list2))) NIL x)) list1))
  )
)

(defun remove-nil(list)
"Remove all nils from the received list"
   (apply #'append (mapcar #'(lambda(x) (if (null x) NIL (list x))) list))
)


(defun get-lowest-f(node)
"Return the node with the lowest f value"
  (cond
   ((null node) nil)
   ((= (length node) 1) (first node))
   (T (let ((recursive-node (get-lowest-f(rest node))))
    (if (< (get-node-f (first node)) (get-node-f recursive-node))
         (first node)
         recursive-node
    )))  
   )
)
 
(defun get-current-node (node)
"Return current node"
  (first node)
)

(defun calculate-node-depth(node)
"Calculate the depth of node"
  (cond
   ((null (get-node-parent node)) 0)
   (T(1+ (calculate-node-depth (get-node-parent node))))
   )
)

(defun get-solution-path (node)
"Return list with all the path from the beginning to the solution"
  (cond 
   ((null (rest node)) '())
   (T (append (get-solution-path (second node)) (list (first node))))
   )
)

(defun expand-node-a*(chosen-node hFunction)
"Expand node using the heuristic function"
  (mapcar #'(lambda (node) (change-position-value node '3 (funcall hFunction node))) (expand-node chosen-node))
)

(defun change-position-value(list position value)
"Change the size of a list in the received position and return new list"
  (cond 
   ((= position 0) (cons value (rest list)))
   (T (cons (first list) (change-position-value (rest list) (1- position) value)))
   )
)

(defun expand-node (node) 
"Expand node checking all possible plays on the board"
  (if (is-board-empty (first node)) nil)
    (remove nil 
      (list
        (attach-parent 0 0 node)
        (attach-parent 0 1 node) 
        (attach-parent 0 2 node) 
        (attach-parent 0 3 node) 
        (attach-parent 0 4 node) 
        (attach-parent 0 5 node) 
        (attach-parent 1 0 node) 
        (attach-parent 1 1 node) 
        (attach-parent 1 2 node) 
        (attach-parent 1 3 node) 
        (attach-parent 1 4 node) 
        (attach-parent 1 5 node)
  ) ) 
)

(defun attach-parent (line column node)
"Attach parent node"
  (if (equal (member (first node) (list (make-play line column (first node))) :test 'equal) nil)
    (construct-node (make-play line column (first node)) node (count-board-pieces (first node)) (1+ (get-node-g node)))
  )
)

(defun penetrance (list)
"Calculate the penetrance"
  (/ (solution-length list) (number-generated-nodes list))
)

(defun solution-node(list)
"Return the solution node from the list"
  (nth (1- (length (first list))) (first list))
)

(defun solution-length (list)
"Return the length of the solution"
 (length (first list))
)

(defun number-expanded-nodes-bfsdfs (list)
"Return the number of nodes that BFS/DFS expanded"
  (third list)
)

(defun branching-factor (list &optional (valor-L (solution-length list)) (valor-T (number-generated-nodes list)) (erro 0.1) (bmin 1) (bmax 10e11))
"Return the ramification factor using the bissection method"
  (let ((bmedio (/ (+ bmin bmax) 2)))
    (cond 
     ((< (- bmax bmin) erro) (/ (+ bmax bmin) 2))
     ((< (f-polinomial bmedio valor-L valor-T) 0) (branching-factor list valor-L valor-T erro bmedio bmax))
     (t (branching-factor list valor-L valor-T erro bmin bmedio))
     )
    )
)

(defun number-generated-nodes (list)
"Return the number of nodes generated"
  (+ (second list) (third list))
)

(defun number-generated-nodes-ida* (list)
"Return the number of nodes generated for IDA* algorithm"
  (+ (second list) (third list) (fifth list))
)

(defun f-polinomial (B L valor-T)
 "Polinomial: B + B^2 + ... + B^L=T"
  (cond
   ((= 1 L) (- B valor-T))
   (T (+ (expt B L) (f-polinomial B (- L 1) valor-T)))
  )
)

(defun get-node-state (node)
"Return the board of the node"
	(first node)
)

(defun recalculate-open(expanded-list open parent-node)
"Used to returning list of open nodes recalculated, assign the highest f to the parent node"
  (mapcar #'(lambda(open-node)
              (let ((list-open (recalculate-node open-node expanded-list)))
                (if (null list-open) open-node (change-parent-node (first list-open) parent-node))
                )
              ) open)                         
)
  
(defun recalculate-closed(expanded-list closed parent-node)
"Used to returning list of closed nodes recalculated, assign the highest f to the parent node"
  (mapcar #'(lambda(closed-node)
              (let ((list-closed (recalculate-node closed-node expanded-list)))
                (if (null list-closed) nil (change-parent-node (first list-closed) parent-node))
                )
              ) closed)                         
)

(defun recalculate-node(node expanded-list)
"If the f value is lower than the value of the expanded-list, it changes the f value otherwise just returns nil"
  (remove-nil (mapcar #'(lambda(expanded-node)
                                      (if (equal (get-node-state node) (get-node-state expanded-node))
                                          (if (>= (get-node-f node) (get-node-f expanded-node))
                                              nil (provide-new-f node (get-node-g expanded-node) (get-node-h expanded-node))
                                           ) nil
                                        )
                                      ) expanded-list))
)

(defun change-parent-node(node parent-node)
"Change parent node with a new value"
  (change-position-value node '1 parent-node)
)

(defun get-node-f(node)
"Return the calculation of F value of the node"
	(+ (get-node-g node) (get-node-h node))
)

(defun get-node-g(node)
"Return the G value from the node"
	(third node)
)

(defun get-node-h(node)
"Return the H value from the node"
	(fourth node)
)

(defun provide-new-f(node newG newH)
"Retun new node with new G and H"
  (change-position-value (change-position-value node '2 newG) '3 newH)
)


(defun number-expanded-nodes-a* (list)
"Number of expanded nodes A*"
  (fourth list)
)

(defun get-node-root-parent (node)
"Return the root parent of the node"
  (cond
    ((null (get-node-parent node)) node)
    (t (get-node-root-parent (get-node-parent node))))
)

(defun get-node-parent (node)
"Return the parent of the node"
	(second node)
)

(defun get-max-value-nodes(open max-value)
"Return list with nodes where the F value is below or equal max-value"
"Recebe uma lista e um dado limiar e retorna uma lista de nos cujo valor F seja <= a esse limiar"
  (remove-nil (mapcar #'(lambda(node) (if (<= (get-node-f node) max-value) node NIL)) open))
)

(defun get-minf-value(list)
"Return the node with the lowest F value"
  (get-node-f (get-lowest-f list))
)