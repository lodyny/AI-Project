(defun construct-node (board parent pieces &optional (g 0) (h 0))
  (list board parent g h pieces)
)

;; (defun filter-nodes (node-list open-list close-list)
;;   (cond ((null node-list) nil)
;;         ((or (node-existsp (car node-list) open-list) (node-existsp (car node-list) close-list)) (filter-nodes (cdr node-list) open-list close-list))
;;         (t (cons (car node-list) (filter-nodes (cdr node-list) open-list close-list)))))

;; (defun node-existsp (node list)
;;   (if (member node list :test 'equal-node) t nil))

(defun bfs (*abertos* &optional (*fechados* '()))
(let ((open-size (length *abertos*)))
  (cond
    ((= open-size 0) nil)
    (T 
      (let* (
            (*noAtual* 
              (first *abertos*))
            (*sucessores*
              (funcall expand-node *noAtual*)
                )
      )
        (if (size-zero *sucessores*)
          (list (get-solution-path *noAtual*) (length *abertos*) (length *fechados*))
          ;; abertos + sucessores filtrados
          (bfs  (concatenate 'list (cdr *abertos*) 
                              (remove-nil
                              (remove-duplicated-nodes
                              *sucessores* (expand-node (first *abertos*)) *fechados*
                              ))) 
                (concatenate 'list *fechados* (list *noAtual*)))
          ))))))

(defun size-zero (sucessores)
  (= (length sucessores) 0)
)

(defun get-solution-path (node)
"Retorna uma lista de estados do root ao goal"
  (cond 
   ((null (get-node-parent node)) (list (first node)))
   (T (append (get-solution-path (get-node-parent node)) (list (first node))))
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
	(apply #'list 
    (make-play 0 0 node) 
    (make-play 0 1 node) 
    (make-play 0 2 node) 
    (make-play 0 3 node) 
    (make-play 0 4 node) 
    (make-play 0 5 node) 
    (make-play 1 0 node) 
    (make-play 1 1 node) 
    (make-play 1 2 node) 
    (make-play 1 3 node) 
    (make-play 1 4 node) 
    (make-play 1 5 node)() 
  )
)

(defun ttg ()
  (expand-node (test-board))
)