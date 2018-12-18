;; Code about the problem/game
;; Developed by Cesar Nero and David Afonso
;; Artificial Intelligence - IPS 2018/2019

(defun empty-board (&optional (linhas 2) (colunas 6))
"Return an empty board"
    (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun test-board ()
"Return an test board"
    '((0 0 1 0 0 0)
      (0 0 0 0 0 0))
)

(defun is-board-empty (board)
"Check if the received board is empty, if yes return T otherwise return Nil"
  (cond ((= (+ (apply #'+ (first board)) (apply #'+ (second board))) 0) T)
          (t Nil)
  )
)

(defun print-board (board &optional (stream t))
"Print a board"
   (not (null (mapcar #'(lambda(l) (format stream "~%~C ~a" #\tab l)) board)))
)

(defun value-of (line column board)
"Return the value on position (line/column) of the received board"
    (nth column (nth line board))
)

(defun make-play (line column board &optional (npieces (value-of line column board)) (first-time 1))
"Make a play on the received board - Operator"
    (cond 
        ((= npieces 0) board)
        ((= first-time 1) (make-play (first (next-position line column)) (first (last (next-position line column)))
                (replace-position line column board) npieces 0))
        (t 
            (cond
                ((= 0 (- npieces 1)) 
                    (check-point line column (make-play (first (next-position line column)) (first (last (next-position line column)))
                    (replace-position line column board (1+ (value-of line column board))) (- npieces 1) 0))
                )
                (t 
                    (make-play (first (next-position line column)) (first (last (next-position line column)))
                    (replace-position line column board (1+ (value-of line column board))) (- npieces 1) 0)
                )
            )
        )
    )
)

(defun next-position (line column)
"Find the next position to play based on line and column received"
    (cond
        ((= line 0)
            (cond
                ((> column 0) (list 0 (1- column)))
                (t '(1 0))  
            )
        )
        (t
            (cond
                ((< column 5) (list 1 (1+ column)))
                (t '(0 5))
            )
        )
    )
)

(defun previous-position (line column)
"Find the previous played position based on line and column received"
    (cond
        ((= line 0)
            (cond
                ((< column 5) (list 0 (1+ column)))
                (t '(1 5))  
            )
        )
        (t
            (cond
                ((> column ) (list 1 (1+ column)))
                (t '(0 5))
            )
        )
    )
)

(defun replace-at (column board &optional (value 0))
"Replace the value inside only one line"
  (cond ((zerop column)  (append (list value) (cdr board)))
        (t (cons (car board) (replace-at (- column 1) (cdr board) value)))
   )
)

(defun replace-position (line column board &optional (value 0))
"Replace the value in the board received in determinated line/column"
    (let ((changed-board (replace-at column (nth line board) value)))
        (cond 
            ((= line 0) (list changed-board (nth 1 board)))
            (t (list (nth 0 board) changed-board))
        )
    )
)

(defun check-point (line column board)
"Check if there is point on the received board, line and column"
    (let ((value (value-of line column board)))
        (cond
            ((or (= 1 value) (= 3 value) (= 5 value)) (replace-position line column board))
            (t board)
        )
    )
)

(defun count-board-pieces (board)
"Return the number of pieces existent in the board"
    (apply '+ (single-list board))
)

(defun single-list (list)
"Cuts the sub-lists and make a one giant list"
    (cond
        ((equal list nil) nil)
        ((atom (car list)) (cons (car list) (single-list (cdr list))))
        ((listp (car list)) (append (car list) (single-list (cdr list))))
    )
)

(defun base-heuristic (node)
"Project Heuristic (Number of pieces on board - Number of pieces to capture on board)"
    (- (count-board-pieces (get-node-state node)) (count-board-pieces (get-node-state (get-node-root-parent node))))
)

(defun best-heuristic (node)
"Improved Heuristic that take into count if is possible to make point in the board or not"
    (- (count-board-pieces (get-node-state node)) (count-board-pieces (get-node-state (get-node-root-parent node))))
)

(defun number-spaces-occupied(board)
"Devolve o numero de espacos ocupados num tabuleiro"
  (apply '+ (mapcar #'(lambda(row) (number-spaces-occupied-row row)) board))
)

(defun number-spaces-occupied-row(row)
"Devolve o numero de espacos ocupados numa linha"
  (apply '+ (mapcar #'(lambda(node) (if (= node 0) 0 1)) row))
)