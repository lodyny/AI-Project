;;;; laboratorio8.lisp
;;;; Disciplina de IA - 2018 / 2019
;;;; Jogo dos animais


;;; Tipo abstrato nó como sendo uma lista com 3 elementos: (valor, ramo-sim, ramo-não)
;; Construtor


;; Seletores


;; input/output

(defun ler-ficheiro ()
  "Função que permite ler um ficheiro"
  (with-open-file (ficheiro "C:/Users/Aluno/IA/Lab8/animais.dat"
                            :direction :input
                            :if-does-not-exist :error)
	(read ficheiro)
   )
)

;; funções auxiliares.

(defun novo-animal(aa &aux (na (ler-animal)))
	(let ((p (ler-pergunta aa na))
		(r (ler-resposta aa)))
		(if (equal r'sim)
			(criar-no p na aa)
			(criar-no p aa na)
		)	
	)
)


(defun terminar-jogo(a)
	(format t "O animal em que pensou é ~S?~C" a #\linefeed)
	(if (equal (read) 'sim)
		(progn (format t "Acertei!")
			a)
		(no-animal a)
	)
)

(defun jogar(no)
	(cond ((null no) (novo-animal nil))
	(no-terminalp no) (terminar-jogo no))
	(t (let ((p (no-pergunta no))
		(s (no-sim no)) (n (no-nao no))
		)
		(format t "~S?~C" p #\linefeed)
		(if (equal (read) 'sim)
			(criar-no p (jogar (no-sim no) n)
			(criar-no p s jogar (no-nao no)))
		)
	)
)
)

(defun ler-animal()
	(format t "Qual é o animal em que penso?~C" #\linefeed)
	(read)
)

(defun ler-pergunta(aa na)
	(format t "O que distingue o ~S de ~S?~C" aa na #\linefeed)
	(read)
)

(defun ler-resposta(a)
	(format t "Se for ~S qual é a resposta?~C" a #\linefeed)
	(read)
)

(defun no-terminalp(no)
	(no (lisp no))
)

(defun criar-no (p s n)
	(list p s n)
)

(defun no-pergunta(no)
	(first no)
)

(defun no-sim(no)
	(second no)
)

(defun no-nao(no)
	(third no)
)

(defun animais(&aux (fname "C:/Users/Perry/Documents/IA/lab8/animais.dat") (fid (open fname :direction :input)))
	(let (bc (read fid)))
		(close fid)
		(let ((nbc (jogar bc)) 
			(fid (open fname :direction :output :if-exists :usersede)))
			(escrever-arvore nbc fid)
			(close fid)
		)
)


(defun ler-arvore(&optional (s *standard-input*))
	(read s)
)

(defun iniciar ()
	(cond((equal(quer-jogar) 'nao)(format t "Adeus!"))
		((equal(pense-num-animal) 'nao) iniciar))
)
		
(defun quer-jogar()
	(format t "Quer jogar o jogo do animais ~C" #\linefeed)
	(read))
	
(defun pense-num-animal()
	(format t "Pense num animal. Está preparado? ~C" #\linefeed)
	(read)
)
	
	


;;; Funções de navegação e construção da árvore, à medida que as perguntas são feitas.


;;; Função de proposta da solução e de reação à resposta do utilizador


;;; Função de construção do nó adicional caso a resposta esteja errada


;;; Função de topo, de preparação do ambiente (abrir ficheiros) e execução das funções de mais alto nível
