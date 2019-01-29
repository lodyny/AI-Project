(defpackage :jogo)

(defun initial-board (&optional (linhas 2) (colunas 6))
"Return an empty board"
    (make-list linhas :initial-element (make-list colunas :initial-element '8))
)

(defun initial-player-board ()
    '(8 8 8 8 8 8)
)

(defun count-board-dif (old-board new-board)
  (-
  (+ (count-board-pieces (first old-board)) (count-board-pieces (second old-board)))
  (+ (count-board-pieces (first new-board)) (count-board-pieces (second new-board)))
  )
)

(defun pieces(&optional (ssv 10) (bsv 10) (cv 15))
"Cria a estrutura de dados das pecas do problema (por defeito (10 10 15))"
  (list ssv bsv cv)
)

(defun make-play (line column board &optional (npieces (value-of line column board)) (first-time 1) (fline line))
"Make a play on the received board - Operator"
    (cond 
        ((= npieces 0) board)
        ((= first-time 1) (make-play (first (next-position line column)) (first (last (next-position line column)))
                (replace-position line column board) npieces 0 fline))
        (t 
            (cond
                ((= 0 (- npieces 1)) 
                    (check-point line column 
                      (make-play 
                        (first (next-position line column)) 
                        (first (last (next-position line column)))
                        (replace-position line column board (1+ (value-of line column board))) 
                        (- npieces 1) 
                        0 
                        fline
                      ) 
                    fline)
                )
                (t 
                    (make-play (
                      first (next-position line column)) 
                      (first (last (next-position line column)))
                      (replace-position line column board (1+ (value-of line column board)))
                      (- npieces 1)
                      0 
                      fline
                    )
                )
            )
        )
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

(defun replace-at (column board &optional (value 0))
"Replace the value inside only one line"
  (cond ((zerop column)  (append (list value) (rest board)))
        (t (cons (first board) (replace-at (- column 1) (rest board) value)))
   )
)

(defun value-of (line column board)
"Return the value on position (line/column) of the received board"
  (nth column (nth line board))
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

(defun is-board-empty (board)
"Check if the received board is empty, if yes return T otherwise return Nil"
  (cond ((= (+ (apply #'+ (first board)) (apply #'+ (second board))) 0) T)
          (t Nil)
  )
)

(defun check-point (line column board original-line)
"Check if there is point on the received board, line and column"
    (let ((value (value-of line column board)))
        (cond
            ((and (/= line original-line)  
                  (or (= 1 value) (= 3 value) (= 5 value))) 
                    (replace-position line column board))
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
        ((atom (first list)) (cons (first list) (single-list (rest list))))
        ((listp (first list)) (append (first list) (single-list (rest list))))
    )
)

(defun print-board (board &optional (stream t))
"Print a board"
   (not (null (mapcar #'(lambda(l) (format stream "~%~C ~a" #\tab l)) board)))
)

(defun change-position-value(list position value)
  "Muda o ï¿½tomo numa certa posiï¿½ï¿½oput de uma lista para um valor recebido e retorna a nova lista"
  (cond 
   ((= position 0) (cons value (rest list)))
   (T (cons (first list) (change-position-value (rest list) (1- position) value)))
   )
)

(defun eval-node (node)
"Project Heuristic (Number of pieces on board - Number of pieces to capture on board)"
    (- (count-board-pieces (get-node-state node)) (count-board-pieces (get-node-state (get-node-root-parent node))))
)