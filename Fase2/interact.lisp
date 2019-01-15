;; Main file used to start the program and load the other files
;; Developed by Cesar Nero and David Afonso
;; Artificial Intelligence - IPS 2018/2019

;; Play:   HUMAN = 1   |   MACHINE = 0
(defun play-hvc (time-limit max-depth player &optional (current-node (construct-node (initial-board) NIL (initial-player-board) (initial-player-board) NIL)))
    (let ((current-board (get-node-state current-node)))
      (let*
        (
          (machine-board (get-node-pieces current-node 0))
          (machine-plays (length (expand-node current-node 0)))
          (human-board (get-node-pieces current-node 1))
          (human-plays (length (expand-node current-node 1)))
        )
        (format t "~%~%----------------------------~%Inicio de uma nova jogada!~%~%")
        (format t "Tabuleiro Atual: ~S~%" current-board)
        (format t "Jogadas possiveis computador: ~S - ~S jogadas possiveis~%" machine-board machine-plays)
        (format t "Jogadas possiveis jogador: ~S - ~S jogadas possiveis~%" human-board human-plays)     

        (cond
        ((or (and (or (null machine-board) (= (length machine-board) 0)) (or (null human-board) (= (length human-board) 0))) (and (= machine-plays 0) (= human-plays 0)))
            (format t "~%~%~%SAIR D~%~%~%~S" current-node))
          ((= player 0)
            ;; MAQUINA A JOGAR
              (format t "~%~%MAQUINA A JOGAR~%")
              (if (or (= machine-plays 0) (null machine-board))
                (progn
                  (format t "~%MAQUINA SEM JOGADAS~%")
                  (play-hvc time-limit max-depth 1 current-node))

                ;; CALCULAR SOLUCAO
                  (let* ((solution-node (negamax current-node time-limit player max-depth))
                        (aux-node (car solution-node))
                        (play (get-play aux-node)))
                    (if (null play)
                      (progn
                        (format t "~%COMPUTADOR SEM JOGADAS POSSIVEIS...~%")
                        (play-hvc time-limit max-depth 1 current-node)
                      )

                      (let* (
                            (new-machine-board (first play))
                            (new-human-board (last play))
                            (new-board (construct-node play NIL new-human-board new-machine-board play))
                            )

                            (play-hvc time-limit max-depth 1 new-board)
                      )
                    )
                  )
                ;; CALCULAR SOLUCAO
              )
            ;; MAQUINA A JOGAR
            )
          (T
            ;; JOGADOR A JOGAR
           (if (or (= human-plays 0) (null human-board))
                 (progn
                   (format t "~%JOGADOR SEM JOGADAS POSSIVEIS...~%")
                   (play-hvc time-limit max-depth 0 current-node)
                  )             
             (let* (
                    (position (get-option 0 5 "Posicao Jogada (0-5): "))
                    (new-board (make-play player position current-board))
                    (new-pieces (last new-board))
                    (new-pieces-m (first new-board))
                    ) 
               (progn
                 (print-board new-board)
                 (play-hvc time-limit max-depth 0 (construct-node new-board NIL new-pieces new-pieces-m position));;1 para testar
                 )
               )        
          )
            ;; JOGADOR A JOGAR
          )
        )
      )
    )
)


(defun start-hvc ()
    (let* 
        (
            (first-player (get-option 1 2 "Primeiro a comecar? (1=Jogador / 2=Computador)"))
            (max-depth (get-option 1 9999999 "Profundidade maxima?"))
            (pc-time (get-option 1 5 "Quanto tempo para o computador pensar em ms? (1000-5000)"))
        )
        (progn 
            ;(write-first-log)
            (if (= first-player 2)
              (play-hvc pc-time max-depth 0)
              (play-hvc pc-time max-depth 1))
        )
    )
)

(defun start (&optional (loadfiles 1))
  (if (= loadfiles 1) (load-files)) 
  (display-menu)
  (let ((opt (get-option 0 1)))
    (ecase opt
      ('0 (display-farewell))
      ('1 (start-hvc))
    )
  )
)

(defun display-menu ()
"Display to the user the main menu"
    (format t "~%~C+------------------------------+" #\tab)
    (format t "~%~C|                              |" #\tab)
    (format t "~%~C|  Bem-vindo(a) ao Adji-boto!  |" #\tab)
    (format t "~%~C|                              |" #\tab)
    (format t "~%~C| 0 - Sair                     |" #\tab)
    (format t "~%~C| 1 - Humano vs Computador     |" #\tab)
    (format t "~%~C| 2 - Computador vs Computador |" #\tab)
    (format t "~%~C|                              |" #\tab)
    (format t "~%~C+------------------------------+" #\tab)
)

(defun display-farewell ()
"Display to the user a goodbye message"
  (format t "~%~CGoodbye!" #\tab)
)

(defun option-text ()
"Ask the user for some input"
  (format t "~%~COpcao: " #\tab)
  (read)
)

(defun option-invalid-text ()
"Display to the user that the option was invalid"
  (progn
    (format t "~COps, opcao invalida!" #\tab)
   )
)

(defun current-time()
"Return the current time on the format of hh:mm:ss"
  (multiple-value-bind (s m h) (get-decoded-time) (list h m s))
)

(defun get-option (min max &optional text)
"Ask the user for some input between the range received (min~max)"
  (if (> (length text) 0) (format t "~%~C~S" #\tab text))
  (let ((opt (option-text)))
    (cond ((not (numberp opt)) (progn (option-invalid-text) (get-option min max)))
          ((or (> opt max) (< opt min)) (progn (option-invalid-text) (get-option min max)))
          (t opt))
  )
)

(defun load-files ()
"Load the projects files needed to the correct function of the program"
  ;(format t "A iniciar processo de carregamento de ficheiros...")
  ;(compile-file (get-default-path 'puzzle 'lisp) :load t)
  ;(compile-file (get-default-path 'procura 'lisp) :load t)
 ; (format t "Carregamento terminado com sucesso, a iniciar jogo...~%")
    (format t "done loading files")
)




; AUXILIAR A MOVER
(defun initial-board (&optional (linhas 2) (colunas 6))
"Return an empty board"
    (make-list linhas :initial-element (make-list colunas :initial-element '8))
)


(defun initial-player-board ()
    '(8 8 8 8 8 8)
)

(defun construct-node (board parent pieces-p1 pieces-p2 playNode &optional (f 0))
"Build a node"
  (list board parent f (list 'P1 pieces-p1) (list 'P2 pieces-p2) playNode)
)

(defun pieces(&optional (ssv 10) (bsv 10) (cv 15))
"Cria a estrutura de dados das pecas do problema (por defeito (10 10 15))"
  (list ssv bsv cv)
)

(defun get-node-state (node)
"Return the board of the node"
	(first node)
)


;; FAZER JOGADA
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

(defun make-play (line column board &optional (npieces (value-of line column board)) (first-time 1) (fline line))
"Make a play on the received board - Operator"
    (cond 
        ((= npieces 0) board)
        ((= first-time 1) (make-play (first (next-position line column)) (first (last (next-position line column)))
                (replace-position line column board) npieces 0 fline))
        (t 
            (cond
                ((= 0 (- npieces 1)) 
                    (check-point line column (make-play (first (next-position line column)) (first (last (next-position line column)))
                    (replace-position line column board (1+ (value-of line column board))) (- npieces 1) 0 fline) fline)
                )
                (t 
                    (make-play (first (next-position line column)) (first (last (next-position line column)))
                    (replace-position line column board (1+ (value-of line column board))) (- npieces 1) 0 fline)
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
  (cond ((zerop column)  (append (list value) (cdr board)))
        (t (cons (car board) (replace-at (- column 1) (cdr board) value)))
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
            ((and (/= line original-line) (or (= 1 value) (= 3 value) (= 5 value))) (replace-position line column board))
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

(defun get-node-g(node)
"Return the G value from the node"
	(third node)
)

(defun get-node-pieces(node player)
"Devolve a estrutura de dados das pecas de um no"
    (if (= player 1)
	      (cadr (nth 3 node))
        (cadr (nth 4 node))
     )
)

(defun print-board (board &optional (stream t))
"Print a board"
   (not (null (mapcar #'(lambda(l) (format stream "~%~C ~a" #\tab l)) board)))
)
;; TERMINAR FAZER JOGADA


;; NEGAMAX
(defun negamax(
               node 
               time-limit                 
               &optional 
               (playing 0)
               (max-depth 50)
               (p-alpha most-negative-fixnum) 
               (p-beta most-positive-fixnum) 
               (start-time (get-universal-time))
               (analised-nodes 1)
               (cuts-number 0)
               )
"Executa a fun��o negamax para um n�"
  (let*  ((expanded-list (order-negamax (funcall 'expand-node node playing)))
          (time-spent (- (get-universal-time) start-time)))
    (cond
     ((or (= max-depth 0) (= (length expanded-list) 0) (>= time-spent time-limit)) ;;se for no folha OU tempo excedido OU profundidade maxima
      (create-solution-node (change-position-value node 2 (* 1 (funcall 'eval-node node))) analised-nodes cuts-number start-time))
     (T 
      (negamax-suc 
       node
       expanded-list
       time-limit
       playing
       max-depth
       p-alpha
       p-beta
       start-time
       analised-nodes
       cuts-number  
       )  
      )
     )
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
          ;(append
          ; (order-negamax (list< (get-node-f (first node-list)) (rest node-list)))
          ; (cons (first node-list) nil)          
          ; (order-negamax (list>= (get-node-f (first node-list)) (rest node-list)))
           ;)
  )
)

(defun negamax-suc(
                   parent-node
                   expanded-list
                   time-limit                 
                   playing
                   max-depth
                   p-alpha
                   p-beta
                   start-time
                   analised-nodes
                   cuts-number
                   )
"NegamaxAuxiliar - Executa a fun��o negamax para os sucessores de um n�"
  (cond
   ((= (length expanded-list) 1) 
    (negamax (invert-node-sign (car expanded-list))
             time-limit
             (- playing)
             (1- max-depth)
             (- p-beta)
             (- p-alpha)
             start-time
             (1+ analised-nodes)
             cuts-number
             )
    )
   (T
    (let*  ((car-solution (negamax (invert-node-sign (car expanded-list))
                                   time-limit
                                   (- playing)
                                   (1- max-depth)
                                   (- p-beta)
                                   (- p-alpha)
                                   start-time
                                   (1+ analised-nodes)
                                   cuts-number
                                   ))
            (car-node (car car-solution))
            (best-value (max-node-f car-node parent-node))
            (alpha (max p-alpha (get-node-f best-value)))
            (car-analised-nodes (get-solution-analised-nodes (cadr car-solution)))       
            (car-cuts (get-solution-cuts (cadr car-solution)))
            )

      (if (>= alpha p-beta)
          ;;corte
          (progn (create-solution-node parent-node car-analised-nodes (1+ car-cuts) start-time))

        ;;n�o corte
        (negamax-suc parent-node
                     (cdr expanded-list)
                     time-limit
                     playing
                     max-depth
                     alpha
                     p-beta
                     start-time
                     car-analised-nodes
                     car-cuts
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

(defun get-solution-analised-nodes(solution-node)
"Retorna a solu��o do n�"
  (nth 0 solution-node)
)

(defun invert-node-sign(node)
"Retorna o n� com a invers�o do seu valor"
  (let* ((node-value (get-node-f node)))
    (change-position-value node 2 (- node-value))
  )
)

(defun get-solution-cuts(solution-node)
"Retorna o n�mero de cortes de um n�"
  (nth 1 solution-node)
)

(defun change-position-value(list position value)
  "Muda o �tomo numa certa posi��oput de uma lista para um valor recebido e retorna a nova lista"
  (cond 
   ((= position 0) (cons value (cdr list)))
   (T (cons (car list) (change-position-value (cdr list) (1- position) value)))
   )
)

(defun eval-node (node)
"Project Heuristic (Number of pieces on board - Number of pieces to capture on board)"
    (- (count-board-pieces (get-node-state node)) (count-board-pieces (get-node-state (get-node-root-parent node))))
)

(defun get-node-root-parent (node)
"Return the root parent of the node"
  (cond
    ((null (get-node-parent node)) node)
    (t (get-node-root-parent (get-node-parent node))))
)

(defun get-node-parent (node)
"Return the parent of the node"
	(cadr node)
)

(defun create-solution-node(play-node analised-nodes cuts-number start-time)
"Constr�i o n� solu��o"
  (list play-node (list analised-nodes cuts-number (get-time-spent start-time)))
)

(defun get-time-spent(start-time)
"Retorna a diferen�a temporal"
  (- (get-universal-time) start-time)
)

(defun get-play(node)
"Retorna a jogada de um n�"
  (let ((parent (get-node-parent node)))
    (cond
     ((null (get-node-parent parent))  (get-play-node node))
     (T (get-play parent))
     )
    )
)

(defun get-play-node(node)
  (nth 5 node)
)