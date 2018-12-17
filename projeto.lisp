;; Main file used to start the program and load the other files
;; Developed by Cesar Nero and David Afonso
;; Artificial Intelligence - IPS 2018/2019

(defun load-files ()
"Load the files from the standard path 'C:/lisp'"
  (format t "A iniciar processo de carregamento de ficheiros...")
  (compile-file "C:/Users/cesarnero/Documents/GitHub/Projecto-IA/puzzle" :load t)
  (compile-file "C:/Users/cesarnero/Documents/GitHub/Projecto-IA/procura" :load t)
  (format t "Carregamento terminado com sucesso, a iniciar jogo...~%")
)

(defun get-problems-path()
"Return the problems.dat path from the standard path 'C:/lisp/problems.dat'"
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "problemas" :type "dat")
)

(defun get-results-path()
"Return the problems.dat path from the standard path 'C:/lisp/results.dat'"
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "results" :type "dat")
)

(defun start (&optional(loadfiles 1))
"Start the program"
    (progn
      (if (= loadfiles 1) (load-files)) 
        (display-menu)
        (let ((opt (ask-option 0 2)))
          (ecase opt
            ('0 (display-farewell))
            ('1 (let ((solution-board (start-search)))
                  solution-board))
            ('2 (let ((board (get-file-board)))
                  (if (listp board) (print-board board))) (start 0))
           )
         )
    )
)

(defun start-search ()
"Start the searching algorithm"
  (progn
    (display-algorithms)
    (let ((opt (ask-option 0 3)))
      (cond ((eq opt 0) (display-farewell))
        (T 
          (let* 
            ((board (get-file-board))
             (node (list (construct-node board nil (count-board-pieces board))))
            )
              (ecase opt
                (1
                   (let ((solution (list (current-time) (bfs node) (current-time) node 'BFS)))
                   (progn (write-statistics solution) solution)
                   )          
                )
                (2
                   (let* ((depth (read-depth))
                                    (solution (list (current-time) (dfs node depth) (current-time) node 'DFS depth)))
                                 (progn (write-statistics solution) solution)
                             )
                 )
              )
          )
        )
      )
    )
  )
)

(defun get-file-board ()
"Read one of the available boards"
  (progn
    (display-file-boards)
    (let ((opt (ask-option 0 (length (read-file-boards)))))
      (cond
        ((eq 0 opt) (start 0))
        (T (nth (1- opt) (read-file-boards)))
      )
    )
  )
)

(defun read-file-boards ()
"Read boards from file"
   (with-open-file (file (get-problems-path) :if-does-not-exist nil)
     (do ((result nil (cons next result))
        	(next (read file nil 'eof) (read file nil 'eof)))
                ((equal next 'eof) (reverse result))
     )
  )
)


;;; INPUT/OUTPUT FUNCTIONS

(defun option-text ()
"Ask the user for some input"
  (format t "~%~COp��o: " #\tab)
  (read)
)

(defun option-invalid-text ()
"Display to the user that the option was invalid"
  (progn
    (format t "~COps, op��o inv�lida!" #\tab)
   )
)

(defun ask-option (min max)
"Ask the user for some input between the range received (min~max)"
  (let ((opt (option-text)))
    (cond ((not (numberp opt)) (progn (option-invalid-text) (ask-option min max)))
          ((or (> opt max) (< opt min)) (progn (option-invalid-text) (ask-option min max)))
          (t opt))
  )
)

(defun read-depth ()	
  (format t "~%~CProfundidade: " #\tab)	
  (read)	
)

(defun display-menu ()
"Display to the user the main menu"
    (format t "~%~C+--------------------------------+" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|   Bem-vindo(a) ao Adji-boto!   |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|     0 - Sair                   |" #\tab)
    (format t "~%~C|     1 - Resolver tabuleiro     |" #\tab)
    (format t "~%~C|     2 - Mostrar um tabuleiro   |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C+--------------------------------+" #\tab)
)

(defun display-algorithms ()
"Display to the user the available algorithms to choose from"
    (format t "~%~C+--------------------------------+" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|      Algoritmos Disponiv�is    |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|     0 - Sair                   |" #\tab)
    (format t "~%~C|     1 - Breath-First Search    |" #\tab)
    (format t "~%~C|     2 - Depth-First Search     |" #\tab)
    (format t "~%~C|     3 - A*                     |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C+--------------------------------+" #\tab)
)

(defun display-file-boards (&optional(i 1) (problems (read-file-boards)))
"Display to the user all available boards to choose from problems.dat"	
  (cond ((null problems) (progn   
                           (format t "~%~C|             0 - Sair                      |" #\tab)
                           (format t "~%~C+-------------------------------------------+" #\tab)))
   (T (progn (if (= i 1) (progn 
                           (format t "~%~C+-------------------------------------------+" #\tab)
                           (format t "~%~C|     ADJI BOTO - Tabuleiros Disponiveis    |" #\tab)))
        (format t "~%~C|             ~a - Tabuleiro ~a               |" #\tab i i) 
        (display-file-boards (+ i 1) (cdr problems))))))

(defun display-farewell ()
"Display to the user a goodbye message"
  (format t "~%~CGoodbye!" #\tab)
)

;;; STATISTICS FUNCTIONS
(defun current-time()
"Return the current time on the format of hh:mm:ss"
(multiple-value-bind (s m h) (get-decoded-time) (list h m s)))

(defun write-statistics (solution)
"Write the solution to the destination file, default: 'C:/lisp/results.dat'"
(let* ((start-time (first solution))
         (solution-path (second solution))
         (end-time (third solution))
         (nboard (fourth solution))
         (search (fifth solution)))
            (with-open-file (file (get-results-path) :direction :output :if-exists :append :if-does-not-exist :create)
                (ecase search
                      ('BFS (write-bfsdfs-stats file solution-path start-time end-time nboard 'BFS ))         
                      ('DFS (let ((depth (sixth solution))) (write-bfsdfs-stats file solution-path start-time end-time nboard 'DFS depth)))
                )
            )
  )
)

(defun write-bfsdfs-stats (stream solution-path start-time end-time nboard search &optional depth)
"Write BFS or DFS Statistics to the file"
  (progn 
    (format stream "~%* Resolucao do Tabuleiro ~a *" nboard)
    (format stream "~%~t> Algoritmo: ~a " search)
    (format stream "~%~t> Inicio: ~a:~a:~a" (first start-time) (second start-time) (third start-time))
    (format stream "~%~t> Fim: ~a:~a:~a" (first end-time) (second end-time) (third end-time))
    (format stream "~%~t> Numero de nos gerados: ~a" (number-generated-nodes solution-path))
    (format stream "~%~t> Numero de nos expandidos: ~a" (number-expanded-nodes-bfsdfs solution-path))
    (format stream "~%~t> Penetrancia: ~F" (penetrance solution-path))
    (format stream "~%~t> Fator de ramificacao media ~F" (branching-factor solution-path))
    (if (eq search 'DFS)
        (format stream "~%~t> Profundidade maxima: ~a" depth))
    (format stream "~%~t> Comprimento da solucao ~a" (solution-length solution-path))
    (format stream "~%~t> Estado Inicial")
    (print-board (first (first solution-path)) stream)
    (format stream "~%~t> Estado Final")
    (print-board (solution-node solution-path) stream)
  )
  )


