(defun load-files ()
  (progn
  (format t "A iniciar processo de carregamento de ficheiros...")
  (compile-file "C:/lisp/puzzle" :load t)
  (compile-file "C:/lisp/procura" :load t)
  (format t "Carregamento terminado com sucesso, a iniciar jogo...~%")
  )
)


(defun get-problems-path()
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "problemas" :type "dat")
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
    (let ((opt (ask-option 0 3)))
      (cond ((eq opt 0) (display-farewell))
        (T 
          (let* 
            ((board-node (get-file-board))
             (board board-node)
             (node (construct-node board nil (count-board-pieces board)))
            )
              (ecase opt
                (1
                  (let ((solution (bfs node)))
                    solution
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
    (format t "~%~C|     1 - Mostrar um tabuleiro   |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C+--------------------------------+" #\tab)
)

(defun display-algorithms ()
    (format t "~%~C+--------------------------------+" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|      Algoritmos Disponivéis    |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C|     0 - Sair                   |" #\tab)
    (format t "~%~C|     1 - Breath-First Search    |" #\tab)
    (format t "~%~C|     2 - Depth-First Search     |" #\tab)
    (format t "~%~C|     3 - A*                     |" #\tab)
    (format t "~%~C|                                |" #\tab)
    (format t "~%~C+--------------------------------+" #\tab)
)

(defun display-farewell ()
  (format t "~%~C Goodbye!" #\tab)
)

