(defun construct-node (board parent pieces &optional (g 0) (h 0))
  (list board parent g h pieces)
)

;; (defun filter-nodes (node-list open-list close-list)

(defun filter-nodes (node-list open-list close-list)
  (cond ((null node-list) nil)
        ((or  (member (car node-list) open-list :test 'equal)  
              (member (car node-list) close-list :test 'equal)) 
                  (filter-nodes (cdr node-list) open-list close-list))
        (t (cons (car node-list) (filter-nodes (cdr node-list) open-list close-list)))))

(defun node-existsp (node list)
  (if (member node list :test 'equal-node) t nil))

(defun equal-node (node1 node2)
  (equal (node-state node1) node2))

(defun node-state (node)
  (car node))

(defun node-board (node-state)
  (car node-state))

(defun node-original (node)
  "Gets the node original root"
    (cond ((null (node-parent node)) node)
(t (node-original (node-parent node)))))

(defun node-pieces (node-state)
(second node-state))

(defun node-print (node)
  "Prints node board, pieces remaining, price, h falue and f value"
  (cond ((null node) nil)
        (t (format t "Original:~%")
           (board-print (node-board (node-state (node-original node)))) 
           (format t "Final:~%")
           (board-print (node-board (node-state node))) 
          ;;  (format t "~%Pieces: ~d~%Depth:~d~%Cost:~d~%F=~d~%H=~d~%" (node-pieces (node-state node)) (node-depth node) (node-cost node) (node-f node) (node-h node))
           )))



(defun bfs (*abertos* &optional (*fechados* '()))
    ;; (loop while (not (null *abertos*)) do
  (let ((no-pieces-left (is-board-empty (get-current-node (first *abertos*))))
   (cond
    ((equal no-pieces-left T) nil)
    (T 
      (let* (
            (*noAtual* 
              (first *abertos*))
            (*sucessores*
              (expand-node (get-current-node *noAtual*)
              ))
      )
        (if (is-board-empty (get-current-node *noAtual*))
          (list (get-current-node *noAtual*) (get-solution-path *fechados*) (length *abertos*) (length *fechados*))
          ;; abertos + sucessores filtrados
          (bfs  (concatenate 
                    'list
                    (filter-nodes
                      *sucessores*
                      *abertos* *fechados*)
                      (cdr *abertos*)
                )
                (concatenate 
                  'list 
                  *fechados*
                  (list *noAtual*)))
          )))))));)
;; (mapcar #'(lambda (expanded-node) (cond ((funcall solution expanded-node) (stop-performance expanded-node)(return expanded-node))))expanded-nodes))))

(defun get-current-node (node)
  (first node)
)

(defun size-zero (sucessores)
  (= (length sucessores) 0)
)

(defun get-solution-path (node)
"Retorna uma lista de estados do root ao goal"
  (cond 
   ((null (get-node-parent node)) (list (first (get-current-node node))))
   (T (append (get-solution-path (rest node)) (list (first (get-current-node node)))))
   )
)

(defun remove-duplicated-nodes(list open closed)
"Recebe uma lista de nos, seguido de uma lista de nos abertos e fechados, retirando da primeira lista os nos ja existentes nas outras"
  (remove-duplicate-values (remove-duplicate-values list open) closed)                
)

(defun remove-duplicate-values (list1 list2)
"Remove da lista1 os valores ja existentes na lista2"
  (if (or (null list1) (null list2)) list1
      (remove-nil (mapcar #'(lambda(x) (if (exists-value x list2) NIL x)) list1))
  )
)

(defun remove-nil(list)
  "Remove todos os nils de ums lista. Retorna nova lista"
   (apply #'append (mapcar #'(lambda(x) (if (null x) NIL (list x))) list))
)

(defun exists-value (value list)
"Verificacao booleana que determina se um valor ja existe numa lista, compara o estado do no"
  (eval (cons 'or (mapcar #'(lambda(n) (equal (first value) (first n))) list)))
)

(defun get-node-parent (node)
"Devolve o no pai de um no"
	(second node)
)

(defun expand-node (node) 
"Expande um no, verificando as posicoes possiveis para cada peca"
  (if (is-board-empty node) nil)
	(remove-duplicates (list
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
  ) :test 'equal) 
)

(defun attach-parent (x y node)
  (construct-node (make-play x y node) node (count-board-pieces node))
)

;;(defun construct-node (board parent pieces &optional (g 0) (h 0))
 
(defun ttg ()
  (node-print (bfs (list test-board))
))