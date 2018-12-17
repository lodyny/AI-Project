(defun construct-node (board parent pieces &optional (g 0) (h 0))
  (list board parent g h pieces)
)


(defun create-node-from-state (board parent heuristic) 
  (let ((g (1+ (caddr parent))) 
        (o-c (funcall heuristic board)))
    (construct-node board parent (1+ (caddr parent)) g o-c (+ g o-c))))


(defun remove-duplicated-nodes(list open closed)
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
  "Remove todos os nils de ums lista. Retorna nova lista"
   (apply #'append (mapcar #'(lambda(x) (if (null x) NIL (list x))) list))
)

(defun bsf (*abertos* &optional (*fechados* '()))
  (let 
      ((expand-size
        (length *abertos*)
      ))
   (cond
    ((= expand-size 0) nil)
    (T 
      (let* (
            (*noAtual* 
              (first *abertos*))
            (*sucessores*
              (expand-node *noAtual*)
            )
      )
        (if (equal (is-board-empty (first *noAtual*)) T)
          (list (get-solution-path *noAtual*) (length *abertos*) (length *fechados*))
          ;; abertos + sucessores filtrados
          (bsf  (concatenate 
                    'list
                    (cdr *abertos*)
                    (remove-nil (remove-duplicated-nodes 
                      *sucessores*
                      *abertos* *fechados*))
                )
                (concatenate 
                  'list 
                  *fechados*
                  (list *noAtual*)
                )
          )
          ))))))

(defun dsf (*abertos* *max-depth* &optional (*fechados* '()))
   (cond
    ((= (length *abertos*) 0) nil)
    ((> (length (get-solution-path (first *abertos*))) *max-depth*) (dsf (rest *abertos*) *max-depth* (concatenate 
                                                                      'list 
                                                                      *fechados*
                                                                      (car *abertos*)
                                                                    )))
    (T 
      (let* (
            (*noAtual* 
              (first *abertos*))
            (*sucessores*
              (expand-node *noAtual*)
            )
      )
        (if (equal (is-board-empty (get-current-node *noAtual*)) T)
          (list (get-solution-path *noAtual*) (length *abertos*) (- (length (get-solution-path (first *abertos*))) 1 ))
          (dsf  (concatenate 
                    'list
                    *sucessores*
                    (cdr *abertos*)
                )
                *max-depth*
                (concatenate 
                  'list 
                  *fechados*
                  (list *noAtual*)
                )
          )
          )))))


  (defun A* (*abertos* heuristic &optional (*fechados* '()) (*captured-pieces* 0))

  (setq *open* (list *abertos*))
  (setq *close* nil)
  (loop while (not (null *open*)) do
        (let* ((*noAtual* (car *open*)) 
               (*unfiltered-nodes* (expand-node *noAtual* )
               (expanded-nodes (filter-nodes *unfiltered-nodes* *open* *close*)))            
          (add-explored 1)
          (add-generate (length expanded-nodes))
          (setq *close* (append *close* (list current-node)))
          (cond ((funcall solution current-node) (stop-performance current-node)(return current-node)))
          ;(setq *open* (append (cdr *open*) expanded-nodes))       
          (setq *open* (ordered-insert-list (cdr *open*) expanded-nodes))
          (setq *open* (filter-nodes-update-open *unfiltered-nodes* *open*))
          ;Failsafe
          ;(setq *open* (qsort *open* #'< cost))
          (stable-sort *open* #'< :key cost)
)))
  ;;  (cond
  ;;   ((= (length *abertos*) 0) nil)
  ;;   (T 
  ;;     (let* (
  ;;           (*noAtual* 
  ;;             (first *abertos*))
  ;;           (*sucessores*
  ;;             (expand-node *noAtual*)
  ;;           )
      ;; )
        ;; (if (equal (is-board-empty (get-current-node *noAtual*)) T)
        ;;   (list (get-solution-path *noAtual*) (length *abertos*) (- (length (get-solution-path (first *abertos*))) 1 ))
        ;;   (dsf  (concatenate 
        ;;             'list
        ;;             *sucessores*
        ;;             (cdr *abertos*)
        ;;         )
        ;;         *max-depth*
        ;;         (concatenate 
        ;;           'list 
        ;;           *fechados*
        ;;           (list *noAtual*)
        ;;         )
        ;;   )
        ;;   )
          ;)))
          ;)

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

(defun expand-node (node &optional (attach-method 'attach-parent)) 
"Expande um no, verificando as posicoes possiveis para cada peca"
  (if (is-board-empty (first node)) nil)
    (remove nil 
      (list
        (funcall attach-method 0 0 node)
        (funcall attach-method 0 1 node) 
        (funcall attach-method 0 2 node) 
        (funcall attach-method 0 3 node) 
        (funcall attach-method 0 4 node) 
        (funcall attach-method 0 5 node) 
        (funcall attach-method 1 0 node) 
        (funcall attach-method 1 1 node) 
        (funcall attach-method 1 2 node) 
        (funcall attach-method 1 3 node) 
        (funcall attach-method 1 4 node) 
        (funcall attach-method 1 5 node)
  ) ) 
)

(defun attach-parent (line column node)
  (if (equal (member (first node) (list (make-play line column (first node))) :test 'equal) nil)
    (construct-node (make-play line column (first node)) node (count-board-pieces (first node)))
  )
)
;;(defun construct-node (board parent pieces &optional (g 0) (h 0))
 
(defun ttg ()
  (node-print (bfs (list test-board))
))