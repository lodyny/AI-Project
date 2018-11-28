;;;; laboratorio6.lisp
;;;; Disciplina de IA - 2018 / 2019
;;;; Ficha de Laboratório nº6 - Apoio ao 1º projeto
;;;; Autor: 


;;; Tabuleiros

(defun tabuleiro-vazio (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro 2x6 (default) com as casas vazias"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '((1 2 3 4 5 6)
    (6 5 4 3 2 1))
)


;;; Exercicios


(defun linha(i-linha tabuleiro)
  (nth i-linha tabuleiro)
)

(defun celula(i-linha i-coluna tabuleiro)
  (nth i-coluna (linha i-linha tabuleiro))
)

(defun tabuleiro-vaziop(tabuleiro)
  (cond ((= (+ (apply #'+ (first tabuleiro)) (apply #'+ (second tabuleiro))) 0) T)
          (t Nil)
  )
)

(defun substituir-posicao(i-linha lista &optional (valor 0))
  (cond ((zerop i-linha)  (append (list valor) (cdr lista)))
        (t (cons (car lista) (substituir-posicao (- i-linha 1) (cdr lista) valor)))
   )
)

(defun substituir(i-linha i-coluna lista &optional (valor 0))
  (let ((lista-alterada (substituir-posicao i-coluna (linha i-linha lista) valor))
       (outra-lista (linha i-linha lista)))
     (cond ((zerop i-linha) (list lista-alterada outra-lista))
           (t (list outra-lista lista-alterada)) 
     )
   )
)


(defun incrementar-posicao(i-linha i-coluna lista)
  (substituir i-linha i-coluna lista (+ (celula i-linha i-coluna lista) 1))
)

(defun distribuir-pecas(num i-linha i-coluna &optional (tabuleiro (tabuleiro-vazio)))
  (cond
   ((zerop num ) nil)
   ((zerop i-linha) (cond 
     ((zerop i-coluna) (append (list (list (1+ i-linha) i-coluna)) (distribuir-pecas (1- num) (1+ i-linha) i-coluna)))
     (t (append (list (list i-linha (1- i-coluna))) (distribuir-pecas (1- num) i-linha (1- i-coluna))))          
     ))
   ((= i-linha 1) (cond 
       ((= i-coluna (1- (list-length tabuleiro))) (append (list (list (1- i-linha) i-coluna)) (distribuir-pecas (1- num) (1- i-linha) i-coluna)))
       (t (append (list (list i-linha (1+ i-coluna))) (distribuir-pecas (1- num) i-linha (1+ i-coluna))))
       )) 
   )
)

(defun operador (i-linha i-coluna tabuleiro)
  (let* ((num-pecas (celula i-linha i-coluna tabuleiro))
         (posicao-pecas (distribuir-pecas num-pecas i-linha i-coluna))         
         (tabuleiro-incrementado (mapcar #'(lambda (posicao)
                      (incrementar-posicao (first posicao) (second posicao) tabuleiro)
                      ) posicao-pecas))
         (ultima-peca (celula (first (last posicao-pecas)) (second (last posicao-pecas)) tabuleiro-incrementado)))         
    (cond ((or (= ultima-peca 1) (= ultima-peca 3) (= ultima-peca 5))
           (substituir (first (last posicao-pecas)) (second (last posicao-pecas)) tabuleiro-incrementado)
           ))

    ) 
)     


(defun operador (i-linha i-coluna tabuleiro)
  (let* ((num-pecas (celula i-linha i-coluna tabuleiro))
         (posicao-pecas (distribuir-pecas num-pecas i-linha i-coluna))         
        )
        (percorrer-posicoes posicao-pecas (substituir i-linha i-coluna tabuleiro 0))
    ) 
)                         
 
(defun percorrer-posicoes(posicoes tabuleiro &aux (i (caar posicoes)) (j (caddr posicoes)))
	(cond ((null posicoes) tabuleiro)
	      ((or (= (length posicoes) 1) (= (celula i j tabuleiro) 1)) (substituir i j tabuleiro 0)) 
	      (t (percorrer-posicoes (cdr posicoes) (incrementar-posicao i j tabuleiro))) 
)
) 