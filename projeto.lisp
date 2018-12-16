(defun load-files ()
  (progn
  (format t "A iniciar processo de carregamento de ficheiros...")
  (compile-file "C:/Users/cesarnero/Documents/GitHub/Projecto-IA/puzzle" :load t)
  (compile-file "C:/Users/cesarnero/Documents/GitHub/Projecto-IA/procura" :load t)
  (format t "Carregamento terminado com sucesso, a iniciar jogo...~%")
  )
)


(defun get-problems-path()
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "problemas" :type "dat")
)

(defun get-results-path()
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "resultados" :type "dat")
)


(defun start ()
    (progn
        (load-files)
        (display-menu)
        (let ((opt (ask-option 0 1)))
          (ecase opt
            ('0 (display-farewell))
            ('1 (let ((solution-board (start-search)))
                  solution-board))
           )
         )
    )
)

(defun start-search ()
  (progn
    (display-algorithms)
    (let ((opt (ask-option 0 6)))
      (cond ((eq opt 0) (display-farewell))
        (T 
          (let* 
            ((board-node (get-file-board))
             (board board-node)
             (node (list (construct-node board nil (count-board-pieces board))))
            )
              (ecase opt
                (1
                   (let ((solution (list (current-time) (bfs node) (current-time) board-node 'BFS)))
                   (progn (write-to-file solution) solution))          
                )
                (2
                   (let* ((depth (read-depth))
                                    (solution (list (current-time) (dfs depth node) (current-time) board-node 'DFS depth)))
                               (progn (write-statistics-file solution) solution)   
                             )
                 )
              )
          )
        )
      )
    )
  )
)

(defun read-depth ()
  (format t "~%~CProfundidade: " #\tab)
  (read)
)

(defun get-file-board ()
  (progn
    (show-file-boards)
    (let ((opt (ask-option 0 (length (read-file-boards)))))
      (cond
        ((eq 0 opt) (test-board))
        (T (nth (1- opt) (read-file-boards)))
      )
    )
  )
)

(defun show-file-boards(&optional(i 1) (problems (read-file-boards)))	
  (cond
     ((null problems) 
      (progn   (format t "~%|                     0 - Sair                              |")
               (format t "~% -----------------------------------------------------------"))
     )
     (T (progn 
          (if (= i 1) (progn (format t "~% -----------------------------------------------------------")
                                            (format t "~%|             ADJI BOTO - Tabuleiros Disponiveis            |")))
          (format t "~%|                     ~a - Tabuleiro ~a                       |" i i) 
          (show-file-boards (+ i 1) (cdr problems))))
  )
)

(defun read-file-boards ()
   (with-open-file (file (get-problems-path) :if-does-not-exist nil)
     (do ((result nil (cons next result))
        	(next (read file nil 'eof) (read file nil 'eof)))
                ((equal next 'eof) (reverse result))
     )
  )
)


;;; INPUT/OUTPUT FUNCTIONS

(defun option-text ()
  (format t "~%~COpção: " #\tab)
  (read)
)

(defun option-invalid-text ()
  (progn
    (format t "~COps, opção inválida!" #\tab)
   )
)

(defun ask-option (min max)
  (let ((opt (option-text)))
    (cond ((not (numberp opt)) (progn (option-invalid-text) (ask-option min max)))
          ((or (> opt max) (< opt min)) (progn (option-invalid-text) (ask-option min max)))
          (t opt))
  )
)

(defun display-menu ()
    (format t "~%~C+--------------------------------+" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|   Bem-vindo(a) ao Adji-boto!   |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|     0 - Sair                   |" #\tab)
    (format t "~%~C|     1 - Resolver tabuleiro     |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C+--------------------------------+" #\tab)
)

(defun display-algorithms ()
    (format t "~%~C+------------------------------------------+" #\tab)
    (format t "~%~C|                                          |" #\tab)
    (format t "~%~C|          Algoritmos Disponivéis          |" #\tab)
    (format t "~%~C|                                          |" #\tab)
    (format t "~%~C|      0 - Voltar                          |" #\tab)
    (format t "~%~C|      1 - Breath-First Search             |" #\tab)
    (format t "~%~C|      2 - Depth-First Search              |" #\tab)
    (format t "~%~C|      3 - A*                              |" #\tab)
    (format t "~%~C|      4 - Simplified Memory A*            |" #\tab)
    (format t "~%~C|      5 - Interactive Deepening A*        |" #\tab)
    (format t "~%~C|      6 - Recursive BestFirst Search      |" #\tab)
    (format t "~%~C|                                          |" #\tab)
    (format t "~%~C+------------------------------------------+" #\tab)
)

(defun display-farewell ()
  (format t "~%~C Cya!" #\tab)
)

(defun current-time()
"Retorna o tempo actual com o formato (h m s)"
  ;;HORAS-MINUTOS-SEGUNDOS
  (multiple-value-bind (s m h) (get-decoded-time)
    (list h m s)
   )
)

(defun write-to-file (solution)
"Escreve, no ficheiro de resultados, a solucao e medidas de desempenho de um determinado problema"
  (let* ((start-time (first solution))
         (solution-path (second solution))
         (end-time (third solution))
         (nboard (fourth solution))
         (search (fifth solution)))
            (with-open-file (file (get-results-path) :direction :output :if-exists :append :if-does-not-exist :create)
                (ecase search
                      ('BFS (write-bfsdfs-statistics file solution-path start-time end-time nboard 'BFS ))         
                )
            )
  )
)

(defun write-bfsdfs-statistics (stream solution-path start-time end-time nboard search &optional depth)
"Escreve a solucao e medidas de desempenho para os algoritmos bfs e dfs"
  (progn 
    (format stream "~%* Resolucao do Tabuleiro ~a *" nboard)
    (format stream "~%~t> Algoritmo: ~a " search)
    (format stream "~%~t> inicio: ~a:~a:~a" (first start-time) (second start-time) (third start-time))
    (format stream "~%~t> Fim: ~a:~a:~a" (first end-time) (second end-time) (third end-time))
    (format stream "~%~t> Estado Inicial")
    (print-board (last (second solution-path)) stream)
    (format stream "~%~t> Estado Final")
    (print-board (first solution-path) stream)
    (format stream "~%")
  )
)