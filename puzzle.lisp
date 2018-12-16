(defun empty-board (&optional (linhas 2) (colunas 6))
    (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun test-board ()
    '((0 0 1 0 0 0)
      (0 0 0 0 0 0))
)

(defun is-board-empty (board)
  (cond ((= (+ (apply #'+ (first board)) (apply #'+ (second board))) 0) T)
          (t Nil)
  )
)

(defun value-of (line column board)
    (nth column (nth line board))
)

(defun make-play (line column board &optional (npieces (value-of line column board)) (first-time 1))
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
  (cond ((zerop column)  (append (list value) (cdr board)))
        (t (cons (car board) (replace-at (- column 1) (cdr board) value)))
   )
)

(defun replace-position (line column board &optional (value 0))
    (let ((changed-board (replace-at column (nth line board) value)))
        (cond 
            ((= line 0) (list changed-board (nth 1 board)))
            (t (list (nth 0 board) changed-board))
        )
    )
)

(defun check-point (line column board)
    (let ((value (value-of line column board)))
        (cond
            ((or (= 1 value) (= 3 value) (= 5 value)) (replace-position line column board))
            (t board)
        )
    )
)

(defun count-board-pieces (board)
    (apply '+ (single-list board))
)

(defun single-list (list)
    (cond
        ((equal list nil) nil)
        ((atom (car list)) (cons (car list) (single-list (cdr list))))
        ((listp (car list)) (append (car list) (single-list (cdr list))))
    )
)
