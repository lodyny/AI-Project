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
        (cond
        ((or  
              (and (or (null machine-board) (= (length machine-board) 0)) 
                  (or (null human-board) (= (length human-board) 0))) 
              (and (= machine-plays 0) (= human-plays 0)))
            (write-end-log current-node))
          ((= player 0)
            ;; <MAQUINA A JOGAR>
              (format t "~%~%~C Ronda da maquina~%" #\tab)
              (if (or (= machine-plays 0) (null machine-board))
                (progn
                  (format t "~%~C Maquina sem jogadas possiveis...~%" #\tab)
                  (play-hvc time-limit max-depth 1 current-node))

                ;; <CALCULAR SOLUCAO>
                  (let* ((solution-node (negamax current-node time-limit player max-depth))
                        (aux-node (first solution-node))
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
                ;; <CALCULAR SOLUCAO/>
              )
            ;; <MAQUINA A JOGAR/>
            )
          (T
            ;; <JOGADOR A JOGAR>
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
                  (play-hvc time-limit max-depth 0 
                    (construct-node new-board NIL new-pieces new-pieces-m position 0 machine-cap
                      (+ human-cap (count-board-dif current-board new-board))))
                )
              )        
            )
            ;; <JOGADOR A JOGAR/>
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
            ;; <MAQUINA 1 A JOGAR>
              (format t "~%~%~C Ronda da maquina 1~%" #\tab)
              (if (or (= machine1-plays 0) (null machine1-board))
                (progn
                  (format t "~%~C Maquina 1 sem jogadas possiveis...~%" #\tab)
                  (play-cvc time-limit max-depth 1 current-node))

                ;; <CALCULAR SOLUCAO>
                  (let* ((solution-node (negamax current-node time-limit player max-depth))
                        (aux-node (first solution-node))
                        (play (get-play aux-node)))
                    (if (null play)
                      (progn
                        (format t "~%~C Maquina 1 sem jogadas possiveis...~%" #\tab)
                        (play-cvc time-limit max-depth 1 current-node)
                      )

                      (let* (
                            (new-machine1-board (first play))
                            (new-machine2-board (last play))
                            (new-board (construct-node play NIL new-machine2-board new-machine1-board 0 0 
                              (+ machine1-cap (count-board-dif current-board play)) machine2-cap))
                            )

                            (progn
                              (write-log solution-node)
                              (play-cvc time-limit max-depth 1 new-board)
                            )
                      )
                    )
                  )
                ;; <CALCULAR SOLUCAO/>
              )
            ;; <MAQUINA 1 A JOGAR/>
            )
          (T
              ;; <MAQUINA 2 A JOGAR>
              (format t "~%~%~C Ronda da maquina 2~%" #\tab)
              (if (or (= machine2-plays 0) (null machine2-board))
                (progn
                  (format t "~%~C Maquina 2 sem jogadas possiveis...~%" #\tab)
                  (play-cvc time-limit max-depth 0 current-node))

                ;; <CALCULAR SOLUCAO>
                  (let* ((solution-node (negamax current-node time-limit player max-depth))
                        (aux-node (first solution-node))
                        (play (get-play aux-node)))
                    (if (null play)
                      (progn
                        (format t "~%~C Maquina 2 sem jogadas possiveis...~%" #\tab)
                        (play-cvc time-limit max-depth 0 current-node)
                      )

                      (let* (
                            (new-machine1-board (first play))
                            (new-machine2-board (last play))
                            (new-board (construct-node play NIL new-machine2-board new-machine1-board 0 0 
                              (+ machine1-cap (count-board-dif current-board play)) machine2-cap))
                            )

                            (progn
                              (write-log solution-node)
                              (play-cvc time-limit max-depth 0 new-board)
                            )
                      )
                    )
                  )
                ;; <CALCULAR SOLUCAO/>
              )
            ;; <MAQUINA 2 A JOGAR/>
          )
        )
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

(defun write-start-log ()
  (progn
       (with-open-file (file (get-default-path 'log 'dat) :direction :output :if-exists :append :if-does-not-exist :create)
          (format file "~%~C----------------- NOVO JOGO ----------------~%" #\tab))    
       (format t "~%~C----------------- NOVO JOGO ----------------~%" #\tab))
)

(defun write-log (solution-node)
 (let* ((current-node (first solution-node))
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
    (format stream "~%~C Nos analisados: ~a " #\tab (get-solution-checked-nodes (second solution-node)))
    (format stream "~%~C Numero cortes: ~a " #\tab (get-solution-cuts (second solution-node)))
    (format stream "~%~C Tempo gasto: ~a " #\tab (get-solution-time-spent (second solution-node)))
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