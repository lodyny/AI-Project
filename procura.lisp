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
          (BFS  (concatenate 'list (cdr open) (remove-nil (remove-duplicated-nodes expanded-list open closed))) (concatenate 'list closed (list current-node)))))))))

(defun DFS (open max-depth &optional (closed '()))
"Depth-First Search"
   (cond ((= (length open) 0) nil)
    ((> (length (get-solution-path (first open))) max-depth) (DFS (rest open) max-depth (concatenate 'list closed (car open))))
    (T (let* ((current-node (first open))(expanded-list (expand-node current-node)))
      (if (equal (is-board-empty (get-current-node current-node)) T)
        (list (get-solution-path current-node) (length open) (- (length (get-solution-path (first open))) 1 ))
          (DFS  (concatenate 'list expanded-list (cdr open)) max-depth (concatenate 'list closed (list current-node))))))))


(defun A*(expandFunction hFunction open &optional (closed '()) (expanded-nodes 0))
"A-STAR Algorithm"
  (cond ((= (length open) 0) nil)
   (T (let* ((chosen-node (get-lowest-f open))
            (expanded-list (funcall expandFunction chosen-node hFunction))
            (recalculated-closed (recalculate-closed expanded-list closed chosen-node))
            (recalculated-open (recalculate-open expanded-list (cdr open) chosen-node))
            (new-open (remove-nil (append recalculated-open (remove-duplicate-values expanded-list recalculated-open) recalculated-closed))))
        (if (=  (length expanded-list) 0)
          (list (get-solution-path chosen-node) (length open) (length closed) expanded-nodes)
          (A* expandFunction hFunction new-open (remove-duplicate-values (concatenate 'list closed (list chosen-node)) recalculated-closed) (1+ expanded-nodes)))))))

(defun IDA*(expandFunction hFunction open &optional (closed '()) (limiar (get-node-f (car open))) (temp-open-nodes-length 0) (expanded-nodes 0))
"Iterative Deepening A-START Algorithm"
  (cond
   ((= (length open) 0) nil)
   (T
     (let ((chosen-node (get-lowest-f (get-limiar-nodes open limiar))))
       (if (null chosen-node)
           (IDA* expandFunction hFunction open closed (get-new-limiar open) (+ temp-open-nodes-length (length open) (length closed)))
         (let*
            ((expanded-list (funcall expandFunction chosen-node hFunction))
            (recalculated-closed (recalculate-closed expanded-list closed chosen-node))
            (recalculated-open (recalculate-open expanded-list (cdr open) chosen-node))
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

(defun number-generated-nodes-ida* (list)
"Retorna o numero de nos gerados em procura informada"
  (+ (second list) (third list) (fifth list))
)

(defun f-polinomial (B L valor-T)
 "B + B^2 + ... + B^L=T"
  (cond
   ((= 1 L) (- B valor-T))
   (T (+ (expt B L) (f-polinomial B (- L 1) valor-T)))
  )
)

(defun get-node-state (node)
"Retorna o estado (tabuleiro) de um no"
	(car node)
)

(defun recalculate-open(expanded-list open father-node)
"Recebe uma lista de nos expandidos, a lista de abertos e o no pai
Caso algum no em expandidos ja exista em abertos, o no em abertos fica com o maior valor f dos dois e muda o pai, caso necessario
Retorna a lista de abertos recalculada"
  (mapcar #'(lambda(open-node)
              (let ((list-open (recalculate-node open-node expanded-list)))
                (if (null list-open) open-node (change-father-node (car list-open) father-node))
                )
              ) open)                         
)
  
(defun recalculate-closed(expanded-list closed father-node)
"Recebe uma lista de nos expandidos, a lista de fechados e o no pai
Caso algum no em expandidos ja exista em fechados, o no em fechados fica com o maior valor f dos dois e muda o pai, caso necessario
Retorna a lista de nos fechados cujo valor alterou, para passarem novamente a abertos"
  (mapcar #'(lambda(closed-node)
              (let ((list-closed (recalculate-node closed-node expanded-list)))
                (if (null list-closed) nil (change-father-node (car list-closed) father-node))
                )
              ) closed)                         
)

(defun recalculate-node(node expanded-list)
"Recebe um no e uma lista de nos expandidos, caso o no exista em expandidos e o seu valor f seja inferior que o valor em expandidos,
o seu valor f e alterado. Caso contrario devolve nil"
  (remove-nil (mapcar #'(lambda(expanded-node)
                                      (if (equal (get-node-state node) (get-node-state expanded-node))
                                          (if (>= (get-node-f node) (get-node-f expanded-node))
                                              nil (provide-new-f node (get-node-g expanded-node) (get-node-h expanded-node))
                                           ) nil
                                        )
                                      ) expanded-list))
)

(defun change-father-node(node newFather)
"Altera o pai de um no, retornando-o com o novo valor"
  (change-position-value node '1 newFather)
)

(defun get-node-f(node)
"Devolve o calculo do valor de F de um no"
	(+ (get-node-g node) (get-node-h node))
)

(defun get-node-g(node)
"Devolve o valor G de um no"
	(nth 2 node)
)

(defun get-node-h(node)
"Devolve o valor H de um no"
	(nth 3 node)
)

(defun provide-new-f(node newG newH)
"Retorna um no com um novo valor G e H"
  (change-position-value (change-position-value node '2 newG) '3 newH)
)


(defun number-expanded-nodes-a* (list)
"Número de nós expandidos a*"
  (fourth list)
)

(defun get-node-root-parent (node)
  (cond
    ((null (get-node-parent node)) node)
    (t (get-node-root-parent (get-node-parent node))))
)

(defun get-node-parent (node)
"Devolve o no pai de um no"
	(cadr node)
)

(defun get-limiar-nodes(open limiar)
"Recebe uma lista e um dado limiar e retorna uma lista de nos cujo valor F seja <= a esse limiar"
  (remove-nil (mapcar #'(lambda(node) (if (<= (get-node-f node) limiar) node NIL)) open))
)

(defun get-new-limiar(list)
"Retorna o valo f mais baixo de uma lista de nos"
  (get-node-f (get-lowest-f list))
)