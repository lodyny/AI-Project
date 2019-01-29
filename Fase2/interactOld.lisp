;; Main file used to start the program and load the other files
;; Developed by Cesar Nero and David Afonso
;; Artificial Intelligence - IPS 2018/2019

(defpackage :p170221080-170221085)

(defun data-directory ()
  "C:/adjiboto/"
)

(compile-file (concatenate 'string (data-directory) "jogo.lisp"))
(compile-file (concatenate 'string (data-directory) "algoritmo.lisp"))

(defun get-default-path (file-name file-type)
"Return the problems.dat path from the standard path 'C:/lisp/problems.dat'"
  (make-pathname :host "c" :directory '(:absolute "adjiboto") :name (string file-name) :type (string file-type))
)

;; Play:   HUMAN = 1   |   MACHINE = 0
(defun play-hvc (time-limit max-depth player &optional (current-node (construct-node (initial-board) NIL (initial-player-board) (initial-player-board) NIL)))
    (let ((current-board (get-node-state current-node)))
      (let*
        (
          (machine-board (get-node-pieces current-node 0))
          (machine-plays (length (expand-node current-node 0)))
          (machine-cap (get-node-captured current-node 0))
          (human-board (get-node-pieces current-node 1))
          (human-plays (length (expand-node current-node 1)))
          (human-cap (get-node-captured current-node 1))
        )

        (format t "~%~C--------------------------------------------" #\tab)
        (format t "~%~C Jogador ~C   Tabuleiro ~C Capturadas" #\tab #\tab #\tab)
        (format t "~%~C Maquina ~C ~S ~C     ~S" #\tab #\tab (first current-board) #\tab machine-cap)
        (format t "~%~C Humano ~C ~S ~C     ~S" #\tab #\tab (second current-board) #\tab human-cap)
        (format t "~%~C--------------------------------------------" #\tab)
        ;(format t "Peças no Tabuleiro:  ~S/~S~%" (count-board-pieces human-board) (count-board-pieces machine-board))
        ;(format t "Jogadas possiveis computador: ~S - ~S jogadas possiveis~%" machine-board machine-plays)
        ;(format t "Jogadas possiveis jogador: ~S - ~S jogadas possiveis~%" human-board human-plays)     

        (cond
        ((or (and (or (null machine-board) (= (length machine-board) 0)) (or (null human-board) (= (length human-board) 0))) (and (= machine-plays 0) (= human-plays 0)))
            (write-end-log current-node))
          ((= player 0)
            ;; MAQUINA A JOGAR
              (format t "~%~%~C Ronda da maquina~%" #\tab)
              (if (or (= machine-plays 0) (null machine-board))
                (progn
                  (format t "~%~C Maquina sem jogadas possiveis...~%" #\tab)
                  (play-hvc time-limit max-depth 1 current-node))

                ;; CALCULAR SOLUCAO
                  (let* ((solution-node (negamax current-node time-limit player max-depth))
                        (aux-node (car solution-node))
                        (play (get-play aux-node)))
                    (if (null play)
                      (progn
                        (format t "~%~C Maquina sem jogadas possiveis...~%" #\tab)
                        (play-hvc time-limit max-depth 1 current-node)
                      )

                      (let* (
                            (new-machine-board (first play))
                            (new-human-board (last play))
                            (new-board (construct-node play NIL new-human-board new-machine-board 0 0 (+ machine-cap (count-board-dif current-board play)) human-cap))
                            )

                            (progn
                              (write-log solution-node)
                              (play-hvc time-limit max-depth 1 new-board)
                            )
                      )
                    )
                  )
                ;; CALCULAR SOLUCAO
              )
            ;; MAQUINA A JOGAR
            )
          (T
            ;; JOGADOR A JOGAR
            (format t "~%~%~C Ronda do humano~%" #\tab)
           (if (or (= human-plays 0) (null human-board))
                 (progn
                   (format t "~%~C Humano sem jogadas possiveis....~%" #\tab)
                   (play-hvc time-limit max-depth 0 current-node)
                  )             
             (let* (
                    (position (get-option 1 6 "Qual a posicao que quer jogar (1-6)?"))
                    (new-board (make-play player (- position 1) current-board))
                    (new-pieces (last new-board))
                    (new-pieces-m (first new-board))
                    ) 
               (progn
                 (print-board new-board)
                 (play-hvc time-limit max-depth 0 (construct-node new-board NIL new-pieces new-pieces-m position 0 machine-cap (+ human-cap (count-board-dif current-board new-board))))
                 )
               )        
          )
            ;; JOGADOR A JOGAR
          )
        )
      )
    )
)

(defun play-cvc (time-limit max-depth player &optional (current-node (construct-node (initial-board) NIL (initial-player-board) (initial-player-board) NIL)))
    (let ((current-board (get-node-state current-node)))
      (let*
        (
          (machine1-board (get-node-pieces current-node 0))
          (machine1-plays (length (expand-node current-node 0)))
          (machine1-cap (get-node-captured current-node 0))
          (machine2-board (get-node-pieces current-node 1))
          (machine2-plays (length (expand-node current-node 1)))
          (machine2-cap (get-node-captured current-node 1))
        )

        (format t "~%~C--------------------------------------------" #\tab)
        (format t "~%~C Jogador ~C   Tabuleiro ~C Capturadas" #\tab #\tab #\tab)
        (format t "~%~C Maquina1 ~C ~S ~C     ~S" #\tab #\tab (first current-board) #\tab machine1-cap)
        (format t "~%~C Maquina2 ~C ~S ~C     ~S" #\tab #\tab (second current-board) #\tab machine2-cap)
        (format t "~%~C--------------------------------------------" #\tab)
        
        (cond
        ((or (and (or (null machine1-board) (= (length machine1-board) 0)) (or (null machine2-board) (= (length machine2-board) 0))) (and (= machine1-plays 0) (= machine2-plays 0)))
            (write-end-log current-node))
          ((= player 0)
            ;; MAQUINA 1 A JOGAR
              (format t "~%~%~C Ronda da maquina 1~%" #\tab)
              (if (or (= machine1-plays 0) (null machine1-board))
                (progn
                  (format t "~%~C Maquina 1 sem jogadas possiveis...~%" #\tab)
                  (play-cvc time-limit max-depth 1 current-node))

                ;; CALCULAR SOLUCAO
                  (let* ((solution-node (negamax current-node time-limit player max-depth))
                        (aux-node (car solution-node))
                        (play (get-play aux-node)))
                    (if (null play)
                      (progn
                        (format t "~%~C Maquina 1 sem jogadas possiveis...~%" #\tab)
                        (play-cvc time-limit max-depth 1 current-node)
                      )

                      (let* (
                            (new-machine1-board (first play))
                            (new-machine2-board (last play))
                            (new-board (construct-node play NIL new-machine2-board new-machine1-board 0 0 (+ machine1-cap (count-board-dif current-board play)) machine2-cap))
                            )

                            (progn
                              (write-log solution-node)
                              (play-cvc time-limit max-depth 1 new-board)
                            )
                      )
                    )
                  )
                ;; CALCULAR SOLUCAO
              )
            ;; MAQUINA 1 A JOGAR
            )
          (T
              ;; MAQUINA 2 A JOGAR
              (format t "~%~%~C Ronda da maquina 2~%" #\tab)
              (if (or (= machine2-plays 0) (null machine2-board))
                (progn
                  (format t "~%~C Maquina 2 sem jogadas possiveis...~%" #\tab)
                  (play-cvc time-limit max-depth 0 current-node))

                ;; CALCULAR SOLUCAO
                  (let* ((solution-node (negamax current-node time-limit player max-depth))
                        (aux-node (car solution-node))
                        (play (get-play aux-node)))
                    (if (null play)
                      (progn
                        (format t "~%~C Maquina 2 sem jogadas possiveis...~%" #\tab)
                        (play-cvc time-limit max-depth 0 current-node)
                      )

                      (let* (
                            (new-machine1-board (first play))
                            (new-machine2-board (last play))
                            (new-board (construct-node play NIL new-machine2-board new-machine1-board 0 0 (+ machine1-cap (count-board-dif current-board play)) machine2-cap))
                            )

                            (progn
                              (write-log solution-node)
                              (play-cvc time-limit max-depth 0 new-board)
                            )
                      )
                    )
                  )
                ;; CALCULAR SOLUCAO
              )
            ;; MAQUINA 2 A JOGAR
          )
        )
      )
    )
)


(defun start-hvc ()
    (let* 
        (
            (first-player (get-option 1 2 "Primeiro a comecar? (1=Jogador / 2=Computador)"))
            (max-depth (get-option 1 999999999 "Profundidade maxima?"))
            (pc-time (get-option 1000 5000 "Quanto tempo para o computador pensar em ms? (1000-5000)"))
        )
        (progn 
            (write-start-log)
            (if (= first-player 2)
              (play-hvc (/ pc-time 1000) max-depth 0)
              (play-hvc (/ pc-time 1000) max-depth 1))
        )
    )
)

(defun start-cvc ()
    (let* 
        (
            (max-depth (get-option 1 999999999 "Profundidade maxima?"))
            (pc-time (get-option 1000 5000 "Quanto tempo para o computador pensar em ms? (1000-5000)"))
        )
        (progn 
            (write-start-log)
            (play-cvc (/ pc-time 1000) max-depth 0)
        )
    )
)

(defun start ()
  (display-menu)
  (let ((opt (get-option 0 2)))
    (ecase opt
      ('0 (display-farewell))
      ('1 (start-hvc))
      ('2 (start-cvc))
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

; AUXILIAR A MOVER
(defun initial-board (&optional (linhas 2) (colunas 6))
"Return an empty board"
    (make-list linhas :initial-element (make-list colunas :initial-element '2))
)


(defun initial-player-board ()
    '(2 2 2 2 2 2)
)

(defun construct-node (board parent pieces-p1 pieces-p2 playNode &optional (f 0) (captured-p1 0) (captured-p2 0))
"Build a node"
  (list board parent f (list 'P1 pieces-p1 captured-p1) (list 'P2 pieces-p2 captured-p2) playNode)
)

(defun pieces(&optional (ssv 10) (bsv 10) (cv 15))
"Cria a estrutura de dados das pecas do problema (por defeito (10 10 15))"
  (list ssv bsv cv)
)

(defun get-node-state (node)
"Return the board of the node"
	(first node)
)

(defun count-board-dif (old-board new-board)
  (-
  (+ (count-board-pieces (first old-board)) (count-board-pieces (second old-board)))
  (+ (count-board-pieces (first new-board)) (count-board-pieces (second new-board)))
  )
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
    (if (= player 0)
	      (cadr (nth 3 node))
        (cadr (nth 4 node))
     )
)

(defun get-node-captured (node player)
    (if (= player 0)
      (first (last (nth 3 node)))
      (first (last (nth 4 node)))
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
"Executa a funï¿½ï¿½o negamax para um nï¿½"
  (let*  ((expanded-list (order-negamax (funcall 'expand-node node playing)))
          (time-spent (- (get-universal-time) start-time)))
    (cond
     ((or (= max-depth 0) (= (length expanded-list) 0) (>= time-spent time-limit))
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
  (cond
   ((= (length expanded-list) 1)
   (if (= 0 playing)
   (negamax (invert-node-sign (car expanded-list))
             time-limit
             1
             (1- max-depth)
             (- p-beta)
             (- p-alpha)
             start-time
             (1+ analised-nodes)
             cuts-number
             )

            (negamax (invert-node-sign (car expanded-list))
             time-limit
             0
             (1- max-depth)
             (- p-beta)
             (- p-alpha)
             start-time
             (1+ analised-nodes)
             cuts-number
             )
   ) 

    )
   (T
    (if (= 0 playing)
    (let*  ((car-solution (negamax (invert-node-sign (car expanded-list))
                                   time-limit
                                   1
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

        ;;nï¿½o corte
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

      (let*  ((car-solution (negamax (invert-node-sign (car expanded-list))
                                   time-limit
                                   0
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

        ;;nï¿½o corte
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

(defun get-solution-time-spent(solution-node)
"Retorna o tempo de execu��o de um n�"
  (nth 2 solution-node)
)

(defun get-solution-analised-nodes(solution-node)
"Retorna a soluï¿½ï¿½o do nï¿½"
  (nth 0 solution-node)
)

(defun invert-node-sign(node)
"Retorna o nï¿½ com a inversï¿½o do seu valor"
  (let* ((node-value (get-node-f node)))
    (change-position-value node 2 (- node-value))
  )
)

(defun get-solution-cuts(solution-node)
"Retorna o nï¿½mero de cortes de um nï¿½"
  (nth 1 solution-node)
)

(defun change-position-value(list position value)
  "Muda o ï¿½tomo numa certa posiï¿½ï¿½oput de uma lista para um valor recebido e retorna a nova lista"
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

(defun create-solution-node (play-node analised-nodes cuts-number start-time)
"Constrï¿½i o nï¿½ soluï¿½ï¿½o"
  (list play-node (list analised-nodes cuts-number (get-time-spent start-time)))
)

(defun get-time-spent (start-time)
"Retorna a diferenï¿½a temporal"
  (- (get-universal-time) start-time)
)

(defun get-play (node)
"Retorna a jogada de um nï¿½"
  (let ((parent (get-node-parent node)))
    (cond
     ((null (get-node-parent parent))  (get-play-node node))
     (T (get-play parent))
     )
    )
)

(defun get-play-node (node)
  (nth 5 node)
)

(defun write-start-log ()
  (progn
       (with-open-file (file (get-default-path 'log 'dat) :direction :output :if-exists :append :if-does-not-exist :create)
          (format file "~%~C----------------- NOVO JOGO ----------------~%" #\tab))    
       (format t "~%~C----------------- NOVO JOGO ----------------~%" #\tab))
)

(defun write-log (solution-node)
 (let* ((current-node (car solution-node))
         (play (get-play current-node))
         (board (get-node-state current-node)))
    (progn
       (with-open-file (file (get-default-path 'log 'dat) :direction :output :if-exists :append :if-does-not-exist :create)
          (write-data file solution-node board play 99))    
       (write-data t solution-node board play 99))
  )
)

(defun write-data (stream solution-node board player position)
(progn 
    (print-board board stream)
    (format stream "~%~C Jogou na posicao: ~a" #\tab position)
    (format stream "~%~C Nos analisados: ~a " #\tab (get-solution-analised-nodes (cadr solution-node)))
    (format stream "~%~C Numero cortes: ~a " #\tab (get-solution-cuts (cadr solution-node)))
    (format stream "~%~C Tempo gasto: ~a " #\tab (get-solution-time-spent (cadr solution-node)))
  )   
)

(defun write-end-log (current-node)
  (progn
       (with-open-file (file (get-default-path 'log 'dat) :direction :output :if-exists :append :if-does-not-exist :create)
            (end-game file current-node))
       (end-game t current-node)
  )
) 

(defun end-game (stream current-node)
  (let ((machine-cap (get-node-captured current-node 0))
        (human-cap (get-node-captured current-node 1)))
    (cond 
      ((> human-cap machine-cap) (format stream "~%~%~C Humano ganhou o jogo com ~S pecas capturadas contra ~S da maquina!~%" #\tab human-cap machine-cap))
      ((> machine-cap human-cap) (format stream "~%~%~C Maquina ganhou o jogo com ~S pecas capturadas contra ~S do humano!~%" #\tab machine-cap human-cap))
      (t (format stream "~%~%~C Houve um empate entre maquina e humano, ambos com ~S pecas capturadas!~%" #\tab human-cap))
    )
    (format stream "~%~%~C----------------- FIM JOGO -----------------~%" #\tab)
  )
)