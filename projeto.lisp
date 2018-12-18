;; Main file used to start the program and load the other files
;; Developed by Cesar Nero and David Afonso
;; Artificial Intelligence - IPS 2018/2019

;; LOAD FILES & CONFIGURATION
(defun get-default-path (file-name file-type)
"Return the problems.dat path from the standard path 'C:/lisp/problems.dat'"
  (make-pathname :host "c" :directory '(:absolute "adjiboto") :name (string file-name) :type (string file-type))
)

(defun load-files ()
"Load the projects files needed to the correct function of the program"
  (format t "A iniciar processo de carregamento de ficheiros...")
  (compile-file (get-default-path 'puzzle 'lisp) :load t)
  (compile-file (get-default-path 'procura 'lisp) :load t)
  (format t "Carregamento terminado com sucesso, a iniciar jogo...~%")
)

;; MAIN FUNCTIONS
(defun start (&optional (loadfiles 1))
"Start the program"
  (if (= loadfiles 1) (load-files)) 
  (display-menu)
  (let ((opt (get-option 0 2)))
    (ecase opt
      ('0 (display-farewell))
      ('1 (let ((board (get-file-board)))
        (if (listp board) (print-board board))) (start 0)
      )
      ('2 (let ((solution-board (begin-search))) solution-board))
    )
  )
)

(defun begin-search ()
"Start one of the searching algorithms"
  (display-algorithms)
  (let ((opt (get-option 0 4)))
    (cond 
      ((eq opt 0) (display-farewell))
      (T (let* 
        ((board (get-file-board))
        (node (list (construct-node board nil (count-board-pieces board)))))
          (ecase opt
            (1
              (let ((solution (list (current-time) (bfs node) (current-time) board)))
                (progn (write-statistics solution 'BFS) solution))          
            )
            (2
              (let* ((depth (get-depth)) (solution (list (current-time) (dfs node depth) (current-time) board depth)))
                (progn (write-statistics solution 'DFS) solution))
            )
            (3
              (let* ((heuristic (get-heuristic)) (solution (list (current-time) (A* 'expand-node-a* heuristic (list (change-position-value (car node) '3 (funcall heuristic (car node))))) (current-time) board)))
                (progn (write-statistics solution 'A*) solution))
            )
            (4
              (let* ((heuristic (get-heuristic)) (solution (list (current-time) (IDA* 'expand-node-a* heuristic (list (change-position-value (car node) '3 (funcall heuristic (car node))))) (current-time) board)))
                (progn (write-statistics solution 'IDA*) solution))
            )
          )
        )
      )
    )
  )
)

;; OUTPUT FUNCTIONS
(defun display-menu ()
"Display to the user the main menu"
    (format t "~%~C+--------------------------------+" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|   Bem-vindo(a) ao Adji-boto!   |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|     0 - Sair                   |" #\tab)
    (format t "~%~C|     1 - Mostrar um tabuleiro   |" #\tab)
    (format t "~%~C|     2 - Resolver tabuleiro     |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C+--------------------------------+" #\tab)
)

(defun display-algorithms ()
"Display to the user the available algorithms to choose from"
    (format t "~%~C+--------------------------------+" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|      Algoritmos Disponiveis    |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|     0 - Sair                   |" #\tab)
    (format t "~%~C|     1 - Breath-First Search    |" #\tab)
    (format t "~%~C|     2 - Depth-First Search     |" #\tab)
    (format t "~%~C|     3 - A*                     |" #\tab)
    (format t "~%~C|     4 - IDA*                   |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C+--------------------------------+" #\tab)
)

(defun display-file-boards (&optional(i 1) (problems (read-file-boards)))
"Display to the user all available boards to choose from the file problemas.dat; if empty only show exit option"	
  (cond ((null problems)
    (progn   
      (format t "~%~C|             0 - Sair                      |" #\tab)
      (format t "~%~C+-------------------------------------------+" #\tab)))
  (T (progn (if (= i 1) (progn 
      (format t "~%~C+-------------------------------------------+" #\tab)
      (format t "~%~C|     ADJI BOTO - Tabuleiros Disponiveis    |" #\tab)))
      (format t "~%~C|             ~a - Tabuleiro ~a               |" #\tab i i) 
      (display-file-boards (+ i 1) (cdr problems))))
  )
)

(defun display-farewell ()
"Display to the user a goodbye message"
  (format t "~%~CGoodbye!" #\tab)
)

(defun display-heuristic()
"Display the option of heuristic to the user. Default one or team made."
  (format t "   ~%+--------------------------------------------+")
  (format t "   ~%|    Qual a heuristica que pretende usar?    |")
  (format t "   ~%|          1 - heuristica Enunciado          |")
  (format t "   ~%|          2 - heuristica Criada             |")
  (format t "   ~%|          0 - Voltar                        |")
  (format t "   ~%+--------------------------------------------+~%~%> ")
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

;; INPUT FUNCTIONS
(defun get-option (min max)
"Ask the user for some input between the range received (min~max)"
  (let ((opt (option-text)))
    (cond ((not (numberp opt)) (progn (option-invalid-text) (get-option min max)))
          ((or (> opt max) (< opt min)) (progn (option-invalid-text) (get-option min max)))
          (t opt))
  )
)

(defun get-depth ()	
"Ask the user for the depth"
  (format t "~%~CProfundidade: " #\tab)	
  (read)	
)

(defun get-heuristic()
"Ask the user for the heuristic"
  (if (not (display-heuristic))
      (let ((opt (read)))
         (cond ((eq opt '0) (start 0))
               ((or (not (numberp opt)) (< opt 0)) (progn (format t "Insira uma opcao valida")) (get-heuristic))
               ((eq opt 1) 'base-heuristic)
               (T 'best-heuristic)
     )))
)

(defun get-file-board ()
"Get all the files and based on the length of the list returning ask the user for input"
  (progn
    (display-file-boards)
    (let ((opt (get-option 0 (length (read-file-boards)))))
      (cond
        ((eq 0 opt) (start 0))
        (T (nth (1- opt) (read-file-boards)))
      )
    )
  )
)

(defun read-file-boards ()
"Read all the boards from the file problemas.dat"
  (with-open-file (file (get-default-path 'problemas 'dat) :if-does-not-exist nil)
    (do ((result nil (cons next result))
      (next (read file nil 'eof) (read file nil 'eof)))
        ((equal next 'eof) (reverse result))
    )
  )
)

;; STATISTICS FUNCTIONS
(defun write-statistics (solution algorithm)
"Write statistics about the solution based on the type of algorithm received, output to the file resultados.dat"
(let* ((begin-time (first solution))
         (solution-path (second solution))
         (finish-time (third solution))
         (nboard (fourth solution))
         (depth (fifth solution)))
            (with-open-file (stream (get-default-path 'resultados 'dat) :direction :output :if-exists :append :if-does-not-exist :create)
              (progn 
                (format stream "~%~C Tabuleiro: ~a " #\tab nboard)
                (format stream "~%~C Algoritmo: ~a " #\tab algorithm)
                (format stream "~%~C Inicio: ~a:~a:~a ~C Fim: ~a:~a:~a" #\tab (first begin-time) (second begin-time) (third begin-time) #\tab (first finish-time) (second finish-time) (third finish-time))
                (format stream "~%~C Numero de nos gerados: ~a" #\tab (number-generated-nodes solution-path))
                (format stream "~%~C Numero de nos expandidos: ~a" #\tab (number-expanded-nodes-bfsdfs solution-path))
                (format stream "~%~C Penetrancia: ~F" #\tab (penetrance solution-path))
                (format stream "~%~C Fator de ramificacao media: ~F" #\tab (branching-factor solution-path))
                (format stream "~%~C Profundidade maxima atingida: ~a" #\tab depth)
                (format stream "~%~C Comprimento da solucao: ~a" #\tab (solution-length solution-path))
                (format stream "~%~C Estado Inicial do Tabuleiro:" #\tab)
                (print-board (first (first solution-path)) stream)
                (format stream "~%~C Estado Final do Tabuleiro:" #\tab)
                (print-board (solution-node solution-path) stream)
                (format stream "~%~%")
              )
            )
  )
)