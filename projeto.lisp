;;;; projeto.lisp
;;;; Disciplina de IA - 2018/2019
;;;; Projeto 1 - Adji Boto
;;;; Autores: Cesar Nero & David Afonso

(defun iniciar ()
  (format t "Bem vindo ao jogo do Adji-Boto! ~C" #\linefeed)
  (let ((nome (ler "Por que nome posso tratar-te?")))
    (format t "Olá ~S, vamos lá começar um jogo de adji-boto, boa sorte!~C" nome #\linefeed)
    ;; TODO LOGIC
    ;;; INICIAR TABULEIRO
    (let ((tabuleiro (iniciar-tabuleiro)))
          (loop until (not (null (jogo-terminado tabuleiro))) do
                ;;; CICLO DE JOGO
                (mostrar-tabuleiro tabuleiro)
                ;;; PEDIR E EXECUTAR JOGADA

                ;;; VERIFICAR SE FEZ PONTOS

                ;;; BOT FAZ JOGADA

                ;;; VERIFICAR SE VEZ PONTOS
                (tabuleiro-vaziop tabuleiro)
          )
    )
    ;;; PEDIR JOGADA
    ;;; EXECUTAR JOGADA
    ;;; VERIFICAR SE FEZ PONTOS
    ;;; BOT FAZ JOGADA
    ;;; VERIFICAR SE FEZ PONTOS
    ;;; REPETE
   )
)

(defun ler (value)
  (format t value)
  (format t "~C >> " #\linefeed)
  (read)
)

(defun iniciar-tabuleiro ()
  '((8 8 8 8 8 8 )
    (8 8 8 8 8 8))
)

(defun linha (nlinha tabuleiro)
  (nth nlinha tabuleiro)
)

(defun celula(nlinha ncoluna tabuleiro)
  (nth ncoluna (linha nlinha tabuleiro))
)

(defun mostrar-tabuleiro (tabuleiro)
  (format t "~CEstado atual do Tabuleiro:~C" #\linefeed #\linefeed)
  (format t "+-----------------+~C" #\linefeed)
  (format t "¦~2,' d¦~2,' d¦~2,' d¦~2,' d¦~2,' d¦~2,' d¦~C" (celula 0 0 tabuleiro) (celula 0 1 tabuleiro) (celula 0 2 tabuleiro) (celula 0 3 tabuleiro) (celula 0 4 tabuleiro) (celula 0 5 tabuleiro) #\linefeed)
  (format t "¦--+--+--+--+--+--¦~C" #\linefeed)
  (format t "¦~2,' d¦~2,' d¦~2,' d¦~2,' d¦~2,' d¦~2,' d¦~C" (celula 1 0 tabuleiro) (celula 1 1 tabuleiro) (celula 1 2 tabuleiro) (celula 1 3 tabuleiro) (celula 1 4 tabuleiro) (celula 1 5 tabuleiro) #\linefeed)
  (format t "+-----------------+~C" #\linefeed)
)

(defun jogo-terminado (tabuleiro)
  (= 0 (celula 0 0 tabuleiro))
)

(defun tabuleiro-vaziop(tabuleiro)
  (cond ((= (+ (apply #'+ (first tabuleiro)) (apply #'+ (second tabuleiro))) 0) T)
          (t Nil)
  )
)