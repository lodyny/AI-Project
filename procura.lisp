;; Code about the searching algorithms
;; Developed by Cesar Nero and David Afonso
;; Artificial Intelligence - IPS 2018/2019


;; Searching Algorithms
(defun bfs (open &optional (closed '()))
"Breath-First Search"
  (let 
      ((expand-size
        (length open)
      ))
   (cond
    ((= expand-size 0) nil)
    (T 
      (let* (
            (current-node 
              (first open))
            (expanded-list
              (expand-node current-node)
            )
      )
        (if (equal (is-board-empty (first current-node)) T)
          (list (get-solution-path current-node) (length open) (length closed))
          ;; abertos + sucessores filtrados
          (bfs  (concatenate 
                    'list
                    (cdr open)
                    (remove-nil (remove-duplicated-nodes 
                      expanded-list
                      open closed))
                )
                (concatenate 
                  'list 
                  closed
                  (list current-node)
                )
          )
          ))))))

(defun dfs (open max-depth &optional (closed '()))
   (cond
    ((= (length open) 0) nil)
    ((> (length (get-solution-path (first open))) max-depth) (dfs (rest open) max-depth (concatenate 
                                                                      'list 
                                                                      closed
                                                                      (car open)
                                                                    )))
    (T 
      (let* (
            (current-node 
              (first open))
            (expanded-list
              (expand-node current-node)
            )
      )
        (if (equal (is-board-empty (get-current-node current-node)) T)
          (list (get-solution-path current-node) (length open) (- (length (get-solution-path (first open))) 1 ))
          (dfs  (concatenate 
                    'list
                    expanded-list
                    (cdr open)
                )
                max-depth
                (concatenate 
                  'list 
                  closed
                  (list current-node)
                )
          )
          )))))


  (defun A* (open heuristic &optional (closed '()) (captured-pieces 0))

  (cond
    ((= (length open) 0) nil)
    (T 
      (let* (
            (current-node 
              (first open))
            (expanded-list
              (expand-node-a current-node ())
            )
      )
))))

;; Node Stuff
;; Node ::= (<state> <parent> <g> <h> <pieces-left>)
(defun construct-node (board parent pieces &optional (g 0) (h 0))
"Build a node"
  (list board parent g h pieces)
)


(defun remove-duplicated-nodes(list open closed)
"Remove from the list "
"Recebe uma lista de nos, seguido de uma lista de nos abertos e fechados, retirando da primeira lista os nos ja existentes nas outras"
  (remove-duplicate-values (remove-duplicate-values list open) closed)                
)

(defun remove-duplicate-values (list1 list2)
"Remove da lista1 os valores ja existentes na lista2"
  (if (or (null list1) (null list2))
      list1
      (remove-nil (mapcar #'(lambda(x) (if (eval (cons 'or (mapcar #'(lambda(n) (equal (car x) (car n))) list2))) NIL x)) list1))
  )
)

(defun remove-nil(list)
"Remove all nils from the received list"
   (apply #'append (mapcar #'(lambda(x) (if (null x) NIL (list x))) list))
)


(defun get-lowest-f(node)
"Retorna o no de uma lista com o valor f mais baixo"
  (cond
   ((= (length node) 1) (first node))
   (T (let ((recursive-node (get-lowest-f(rest node))))
    (if (< (get-node-f (first node)) (get-node-f recursive-node))
         (first node)
         recursive-node
    )))  
   )
)
 
(defun get-current-node (node)
  (first node)
)

(defun calculate-node-depth(node)
"Calcula a profundidade de um no"
  (cond
   ((null (get-node-parent node)) 0)
   (T(1+ (calculate-node-depth (get-node-parent node))))
   )
)

(defun get-solution-path (node)
"Retorna uma lista de estados do root ao goal"
  (cond 
   ((null (rest node)) '())
   (T (append (get-solution-path (second node)) (list (car node))))
   )
)

(defun get-node-parent (node)
"Devolve o no pai de um no"
	(second node)
)

(defun expand-node-a*(chosen-node hFunction)
"Expande um no, calculando o valor heuristico dos sucessores antes de os retornar"
  (mapcar #'(lambda (node) (change-position-value node '3 (funcall hFunction node))) (expand-node chosen-node))
)

(defun change-position-value(list position value)
  "Muda o �tomo numa certa posi��o de uma lista para um valor recebido e retorna a nova lista"
  (cond 
   ((= position 0) (cons value (cdr list)))
   (T (cons (car list) (change-position-value (cdr list) (1- position) value)))
   )
)

(defun base-heuristic(node)
"Heuristica dada no enunciado"
  (let ((occupied-spaces (number-spaces-occupied (get-node-state node))))
    (- (- (* 14 14) occupied-spaces) occupied-spaces))
)

(defun expand-node-a (node pieces &optional (heuristic 'base-heuristic)) 
"Expande um no, verificando as posicoes possiveis para cada peca"
  (if (is-board-empty (first node)) nil)
    (remove nil 
      (list
        (attach-parent-a 0 0 node pieces heuristic)
        (attach-parent-a 0 1 node pieces heuristic) 
        (attach-parent-a 0 2 node pieces heuristic) 
        (attach-parent-a 0 3 node pieces heuristic) 
        (attach-parent-a 0 4 node pieces heuristic) 
        (attach-parent-a 0 5 node pieces heuristic) 
        (attach-parent-a 1 0 node pieces heuristic) 
        (attach-parent-a 1 1 node pieces heuristic) 
        (attach-parent-a 1 2 node pieces heuristic) 
        (attach-parent-a 1 3 node pieces heuristic) 
        (attach-parent-a 1 4 node pieces heuristic) 
        (attach-parent-a 1 5 node pieces heuristic)
  ) ) 
)

(defun attach-parent-a (line column node pieces heuristic)
  (if (equal (member (first node) (list (make-play line column (first node))) :test 'equal) nil)
     (create-node-from-state board parent heuristic (count-board-pieces (first node)) pieces) 
    ;; (create-node-from-state (make-play line column (first node)) node (count-board-pieces (first node)))
  ))

  
(defun create-node-from-state (board parent heuristic o pieces) 
  (let ((g (1+ (caddr parent))))
    (construct-node board parent g (- o (- pieces o)) ))
)

(defun expand-node (node) 
"Expande um no, verificando as posicoes possiveis para cada peca"
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
  (if (equal (member (first node) (list (make-play line column (first node))) :test 'equal) nil)
    (construct-node (make-play line column (first node)) node (count-board-pieces (first node)))
  )
)
 
(defun ttg ()
  (node-print (bfs (list test-board))
))


(defun penetrance (list)
"Penetrancia"
  (/ (solution-length list) (number-generated-nodes list))
)

(defun solution-node(list)
"Nó solução"
  (nth (1- (length (car list))) (car list))
)

(defun solution-length (list)
"Devolve o comprimento de uma  solucao"
 (length (car list))
)

(defun number-expanded-nodes-bfsdfs (list)
"Número de nós expandidos bfs and dfs"
  (third list)
)

(defun branching-factor (list &optional (valor-L (solution-length list)) (valor-T (number-generated-nodes list)) (erro 0.1) (bmin 1) (bmax 10e11))
"Devolve o factor de ramificacao, executando o metodo da bisseccao"
  (let ((bmedio (/ (+ bmin bmax) 2)))
    (cond 
     ((< (- bmax bmin) erro) (/ (+ bmax bmin) 2))
     ((< (f-polinomial bmedio valor-L valor-T) 0) (branching-factor list valor-L valor-T erro bmedio bmax))
     (t (branching-factor list valor-L valor-T erro bmin bmedio))
     )
    )
)

(defun number-generated-nodes (list)
"Retorna Número de nós gerados"
  (+ (second list) (third list))
)

(defun f-polinomial (B L valor-T)
 "B + B^2 + ... + B^L=T"
  (cond
   ((= 1 L) (- B valor-T))
   (T (+ (expt B L) (f-polinomial B (- L 1) valor-T)))
  )
)
