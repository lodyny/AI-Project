(defpackage :algoritmo)

(defun construct-node (board parent pieces-p1 pieces-p2 playNode &optional (f 0) (captured-p1 0) (captured-p2 0))
"Build a node"
  (list board parent f (list 'P1 pieces-p1 captured-p1) (list 'P2 pieces-p2 captured-p2) playNode)
)

(defun negamax(
                node 
                time-limit                 
                &optional 
                (playing 0)
                (max-depth 50)
                (p-alpha most-negative-fixnum) 
                (p-beta most-positive-fixnum) 
                (start-time (get-universal-time))
                (checked-nodes 1)
                (cuts-number 0)
               )
  (let*  ((expanded-list (order-negamax (funcall 'expand-node node playing)))
          (time-spent (- (get-universal-time) start-time)))
    (cond
     ((or (= max-depth 0) (= (length expanded-list) 0) (>= time-spent time-limit))
      (create-solution-node 
        (change-position-value node 2 (* 1 (funcall 'eval-node node)))
        checked-nodes
        cuts-number
        start-time
        ) 
      )
     (T 
      (negamax-children 
        node
        expanded-list
        time-limit
        playing
        max-depth
        p-alpha
        p-beta
        start-time
        checked-nodes
        cuts-number  
       )  
      )
    )
  )
)


(defun get-node-state (node)
"Return the board of the node"
	(first node)
)

(defun expand-node (node player) 
"Expand node checking all possible plays on the board"
  (if (is-board-empty (first node)) nil)
    (remove nil 
        (list
        (attach-parent player 0 node) 
        (attach-parent player 1 node) 
        (attach-parent player 2 node) 
        (attach-parent player 3 node) 
        (attach-parent player 4 node) 
        (attach-parent player 5 node)
        )
  ) 
)

(defun attach-parent (line column node)
"Attach parent node"
  (if (equal (member (first node) (list (make-play line column (first node))) :test 'equal) nil)
    (construct-node (make-play line column (first node)) node
      (get-node-pieces (make-play line column (first node)) 1)
      (get-node-pieces (make-play line column (first node)) 2)
      (make-play line column (first node))
    )
  )
)

(defun get-node-g(node)
"Return the G value from the node"
	(third node)
)

(defun get-node-pieces(node player)
"Devolve a estrutura de dados das pecas de um no"
    (if (= player 0)
	      (second (nth 3 node))
        (second (nth 4 node))
     )
)

(defun get-node-captured (node player)
    (if (= player 0)
      (first (last (nth 3 node)))
      (first (last (nth 4 node)))
    )
)




  (defun order-negamax (node-list)
"Ordena uma lista executando o algoritmo quicksort"
  (if (null node-list)
       nil
          (append
           (order-negamax (list>= (get-node-f (first node-list)) (rest node-list)))
           (cons (first node-list) nil)
           (order-negamax (list< (get-node-f (first node-list)) (rest node-list)))
           )
  )
)

(defun negamax-children(
                          parent-node
                          expanded-list
                          time-limit                 
                          playing
                          max-depth
                          p-alpha
                          p-beta
                          start-time
                          checked-nodes
                          cuts-number
                        )
  (cond
   ((= (length expanded-list) 1)
   (if (= 0 playing)
   (negamax (invert-node-sign (first expanded-list))
             time-limit
             1
             (1- max-depth)
             (- p-beta)
             (- p-alpha)
             start-time
             (1+ checked-nodes)
             cuts-number
             )

            (negamax (invert-node-sign (first expanded-list))
             time-limit
             0
             (1- max-depth)
             (- p-beta)
             (- p-alpha)
             start-time
             (1+ checked-nodes)
             cuts-number
             )
   ) 

    )
   (T
    (if (= 0 playing)
    (let*  ((car-solution 
              (negamax  
                (invert-node-sign (first expanded-list))
                time-limit
                1
                (1- max-depth)
                (- p-beta)
                (- p-alpha)
                start-time
                (1+ checked-nodes)
                cuts-number
              )
            )
            (car-node (first car-solution))
            (best-value (max-node-f car-node parent-node))
            (alpha (max p-alpha (get-node-f best-value)))
            (first-checked-nodes (get-solution-checked-nodes (second car-solution)))       
            (car-cuts (get-solution-cuts (second car-solution)))
            )

      (if (>= alpha p-beta)
          ;;<corte>
          (progn (create-solution-node 
                    parent-node 
                    first-checked-nodes 
                    (1+ car-cuts) 
                    start-time)
                  )

          ;; <corte/>
        (negamax-children parent-node
                     (rest expanded-list)
                     time-limit
                     playing
                     max-depth
                     alpha
                     p-beta
                     start-time
                     first-checked-nodes
                     car-cuts
                     )
        )
      )

      (let*  ((car-solution (negamax (invert-node-sign (first expanded-list))
                                   time-limit
                                   0
                                   (1- max-depth)
                                   (- p-beta)
                                   (- p-alpha)
                                   start-time
                                   (1+ checked-nodes)
                                   cuts-number
                                   ))
            (car-node (first car-solution))
            (best-value (max-node-f car-node parent-node))
            (alpha (max p-alpha (get-node-f best-value)))
            (first-checked-nodes (get-solution-checked-nodes (second car-solution)))       
            (car-cuts (get-solution-cuts (second car-solution)))
            )

      (if (>= alpha p-beta)
          ;;corte
          (progn (create-solution-node parent-node first-checked-nodes (1+ car-cuts) start-time))

        ;;nï¿½o corte
        (negamax-children parent-node
                     (rest expanded-list)
                     time-limit
                     playing
                     max-depth
                     alpha
                     p-beta
                     start-time
                     first-checked-nodes
                     car-cuts
                     )
        )
      )
      )
    )
   )
  )

  (defun list< (N node-list)
"Auxiliar quicksort para valores menores"
  (if (null node-list)
       nil
      (if (< (get-node-f(first node-list)) N)
          (cons 
             (first node-list)
             (list< N (rest node-list))
          )
          (list< N (rest node-list))
       )
  )
)

(defun list>= (N node-list)
"Auxiliar quicksort para valores maiores"
  (if (null node-list)
       nil
      (if (>= (get-node-f(first node-list)) N)
          (cons 
             (first node-list)
             (list>= N (rest node-list))
          )
          (list>= N (rest node-list))
       )
  )
)

(defun max-node-f(a b)
  (let ((value-a (get-node-f a))
        (value-b (get-node-f b)))
  (if (> value-a value-b) a b)
  )
)

(defun get-node-f(node)
"Devolve o valor F de um no"
	(nth 2 node)
)

(defun get-solution-time-spent(solution-node)
"Retorna o tempo de execu��o de um n�"
  (nth 2 solution-node)
)

(defun get-solution-checked-nodes(solution-node)
"Retorna a soluï¿½ï¿½o do nï¿½"
  (nth 0 solution-node)
)

(defun invert-node-sign(node)
"Retorna o nï¿½ com a inversï¿½o do seu valor"
  (let* ((node-value (get-node-f node)))
    (change-position-value node 2 (- node-value))
  )
)

(defun get-solution-cuts(solution-node)
"Retorna o nï¿½mero de cortes de um nï¿½"
  (nth 1 solution-node)
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

(defun create-solution-node (play-node checked-nodes cuts-number start-time)
"Constrï¿½i o nï¿½ soluï¿½ï¿½o"
  (list play-node (list checked-nodes cuts-number (get-time-spent start-time)))
)

(defun get-time-spent (start-time)
"Retorna a diferenï¿½a temporal"
  (- (get-universal-time) start-time)
)

(defun get-play (node)
"Retorna a jogada de um nï¿½"
  (let ((parent (get-node-parent node)))
    (cond
     ((null (get-node-parent parent))  (get-play-node node))
     (T (get-play parent))
     )
    )
)

(defun get-play-node (node)
  (nth 5 node)
)