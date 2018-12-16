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


(defun bfs (*abertos* &optional (*fechados* '()))
    ;; (loop while (not (null *abertos*)) do
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
          (list (get-solution-path *noAtual*) (get-current-node *noAtual*) (length *abertos*) (length *fechados*))
          ;; abertos + sucessores filtrados
          (bfs  (concatenate 
                    'list
                    (cdr *abertos*)
                    (filter-nodes
                      *sucessores*
                      *abertos* *fechados*)
                )
                (concatenate 
                  'list 
                  *fechados*
                  (list *noAtual*)
                )
          )
          ))))))


(defun dfs (*abertos* *max-depth* &optional (*fechados* '()))
    ;; (loop while (not (null *abertos*)) do
  (let 
      ( (expand-size
          (length *abertos*)
        )
        (actual-depth
          (length *fechados*)
        )
      )
   (cond
    ((= expand-size 0) nil)
    ((> actual-depth *max-depth*) (dfs (rest *abertos*) *max-depth* (concatenate 
                                                                      'list 
                                                                      *fechados*
                                                                      (car *abertos*)
                                                                    )))
    (T 
      (let* (
            (*noAtual* 
              (first *abertos*))
            (*sucessores*
              (expand-node *noAtual*
              ))
      )
        (if (equal (is-board-empty (get-current-node *noAtual*)) T)
          (list (get-solution-path *noAtual*) (get-current-node *noAtual*) (length *abertos*) (length *fechados*))
          ;; abertos + sucessores filtrados
          (dfs  (concatenate 
                    'list
                    (filter-nodes
                      *sucessores*
                      *abertos* *fechados*)
                      (cdr *abertos*)
                )
                *max-depth*
                (concatenate 
                  'list 
                  *fechados*
                  (list *noAtual*)
                )
          )
          ))))))
  
          
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
   ((null (get-node-parent node)) '())
   (T (list (get-current-node node) (get-solution-path (rest node))))
   )
)

(defun get-node-parent (node)
"Devolve o no pai de um no"
	(second node)
)

(defun expand-node (node) 
"Expande um no, verificando as posicoes possiveis para cada peca"
  (if (is-board-empty (first node)) nil)
 (remove nil (list
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
;;(defun construct-node (board parent pieces &optional (g 0) (h 0))
 
(defun ttg ()
  (node-print (bfs (list test-board))
))