# Manual Técnico

![Cover](https://i.imgur.com/bNNqm4e.png)

## Realizado por

- César Nero nº 170221080
- David Afonso nº 170221081

## Docentes

- Prof. Hugo Silva
- Prof. Joaquim Filipe

## Inteligência Artificial - IPS 2018/2019

1. [Introdução](#p1)
2. [Algoritmo implementado](#p2)
    1. [Funções auxiliares](#p21)
3. [Tipos abstratos](#p3)
    1. [Tabuleiro](#p31)
    2. [Nós](#p32)
    3. [Expanção](#p33)
4. [Limitações](#p4)
5. [Demonstração de jogo](#p5)

# 1. Introdução <a name="p1"></a>

Este projecto pretende dar continuidade ao raciocinio desenvolvido na primeira fase do projecto sobre o jogo Adjiboto. O manual em questão, assim como o programa no qual é baseado, promove não a resolução de tabuleiros singulares, mas a capacidade de jogadas em turnos, sejam estas provenientes por parte de um jogador ou de um outro computador.

O programa está dividido em 3 partes, divisão está de acordo com o enunciado do projecto:

* Uma parte para o algoritmo AlfaBeta ou Negamax (algoritmo.lisp).
* Outra  que  contém  as  funções  que  permitem  escrever  e  ler  em  ﬁcheiros  e  tratar  da  interação  com  o  utilizador (interact.lisp).
* Uma terceira parte corresponde aos operadores do jogo (jogo.lisp).

# 2. Algoritmo implementado <a name="p2"></a>

O algortimo implementado para a resolução do problema foi o negamax com uso de cortes alfabeta.
Pseudo-código do algoritmo segundo os slides da aula:

~~~~
;;; argumentos: nó n, profundidade d, cor c
;;; b = ramificação (número de sucessores)
 function negamax (n, d, c &aux bestValue)
     if d = 0 ou n é terminal
         return c * valor heuristico de n
     bestValue := −∞
     para cada sucessor de n (n1, ..., nk,..., nb)
        bestValue := max(bestValue, −negamax(nk, d−1, −c) )
     return bestValue
~~~~

Código de implementação em LISP

```lisp
(defun negamax(
                node 
                time-limit                 
                &optional 
                (playing 0)
                (max-depth 50)
                (p-alpha most-negative-fixnum) 
                (p-beta most-positive-fixnum) 
                (start-time (get-universal-time))
                (checked-nodes 1)
                (cuts-number 0)
               )
"Executa a funï¿½ï¿½o negamax para um nï¿½"
  (let*  ((expanded-list (order-negamax (funcall 'expand-node node playing)))
          (time-spent (- (get-universal-time) start-time)))
    (cond
     ((or (= max-depth 0) (= (length expanded-list) 0) (>= time-spent time-limit))
      (create-solution-node 
        (change-position-value node 2 (* 1 (funcall 'eval-node node)))
        checked-nodes
        cuts-number
        start-time
      )
      )
      (T
        (negamax-children 
          node
          expanded-list
          time-limit
          playing
          max-depth
          p-alpha
          p-beta
          start-time
          checked-nodes
          cuts-number  
        )  
      )
    )
  )
)
```
# 2.1 Funções auxiliares <a name="p21"></a>

Função auxiliar para os sucessores de um nó

```lisp

(defun negamax-children(
                          parent-node
                          expanded-list
                          time-limit                 
                          playing
                          max-depth
                          p-alpha
                          p-beta
                          start-time
                          checked-nodes
                          cuts-number
                        )
  (cond
   ((= (length expanded-list) 1)
   (if (= 0 playing)
   (negamax (invert-node-sign (first expanded-list))
             time-limit
             1
             (1- max-depth)
             (- p-beta)
             (- p-alpha)
             start-time
             (1+ checked-nodes)
             cuts-number
             )

            (negamax (invert-node-sign (first expanded-list))
             time-limit
             0
             (1- max-depth)
             (- p-beta)
             (- p-alpha)
             start-time
             (1+ checked-nodes)
             cuts-number
             )
   ) 

    )
   (T
    (if (= 0 playing)
    (let*  ((car-solution 
              (negamax  
                (invert-node-sign (first expanded-list))
                time-limit
                1
                (1- max-depth)
                (- p-beta)
                (- p-alpha)
                start-time
                (1+ checked-nodes)
                cuts-number
              )
            )
            (car-node (first car-solution))
            (best-value (max-node-f car-node parent-node))
            (alpha (max p-alpha (get-node-f best-value)))
            (first-checked-nodes (get-solution-checked-nodes (second car-solution)))       
            (car-cuts (get-solution-cuts (second car-solution)))
            )

      (if (>= alpha p-beta)
          ;; <corte>
          (progn (create-solution-node 
                    parent-node 
                    first-checked-nodes 
                    (1+ car-cuts) 
                    start-time)
                  )

          ;; <corte/>
        (negamax-children parent-node
                     (rest expanded-list)
                     time-limit
                     playing
                     max-depth
                     alpha
                     p-beta
                     start-time
                     first-checked-nodes
                     car-cuts
                     )
        )
      )

      (let*  ((car-solution (negamax (invert-node-sign (first expanded-list))
                                   time-limit
                                   0
                                   (1- max-depth)
                                   (- p-beta)
                                   (- p-alpha)
                                   start-time
                                   (1+ checked-nodes)
                                   cuts-number
                                   ))
            (car-node (first car-solution))
            (best-value (max-node-f car-node parent-node))
            (alpha (max p-alpha (get-node-f best-value)))
            (first-checked-nodes (get-solution-checked-nodes (second car-solution)))       
            (car-cuts (get-solution-cuts (second car-solution)))
            )

      (if (>= alpha p-beta)
          ;; <corte>
          (progn (create-solution-node parent-node first-checked-nodes (1+ car-cuts) start-time))

          ;; <corte/>
        (negamax-children parent-node
                     (rest expanded-list)
                     time-limit
                     playing
                     max-depth
                     alpha
                     p-beta
                     start-time
                     first-checked-nodes
                     car-cuts
                     )
         )
       )
     )
   )
  )
)

```

# 3. Tipos abstratos <a name="p3"></a>

## 3.1 Tabuleiro <a name="p31"></a>

Um tabuleiro é composto por duas listas, cada uma delas contendo 6 numeros, estes representativos do número de peças na posição correspondente.
A construção do nosso tabuleiro tem como base a seguinte função:

```lisp
(defun initial-board (&optional (lines 2) (columns 6))
"Return an empty board"
    (make-list lines :initial-element (make-list columns :initial-element '2))
)
```

## 3.2 Nós <a name="p32"></a>

Um nó é constituído por pelo seu nó de origem, peças em cada uma das linhas e a jogada efectuada.
A construção de um nó tem como base a seguinte função:

```lisp
(defun construct-node (board parent pieces-p1 pieces-p2 playNode &optional (f 0) (captured-p1 0) (captured-p2 0))
  (list board parent f (list 'P1 pieces-p1 captured-p1) (list 'P2 pieces-p2 captured-p2) playNode)
)
```

## 3.3 Expanção <a name="p33"></a>

Para a expanção de um nó, utilizamos um encapsulamento inicial que vai compilar todas as jogadas possíveis apartir de uma origem:

```lisp
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
```

Essa função por sua vez irá chamar a função attach-parent, que tem como base um nó definido por uma linha e coluna, que é testado. O tabuleiro resultante da jogada sobre esse nó é então devolvido:

```lisp
(defun attach-parent (line column node)
  (if (equal (member (first node) (list (make-play line column (first node))) :test 'equal) nil)
    (construct-node (make-play line column (first node)) node
      (get-node-pieces (make-play line column (first node)) 1)
      (get-node-pieces (make-play line column (first node)) 2)
      (make-play line column (first node))
    )
  )
)
```

# 4. Limitações <a name="p4"></a>

Em resumo, três limitações se destacam. De acordo com o enunciado do projecto, a memoização deveria ter sido implementada. No entanto, após várias tentativas e dificuldades encontradas, acabámos por não implementar a mesma, por pois possivelmente iria causar um atraso na entrega do projecto, sendo que o valor da implementação desta função seria perdido de ambas as formas.
De resto, deparámo-nos com duas situções:
* Aquando a jogada por parte de uma máquina, a indicação de posição da mesma não está a ser correctamente representada.
* No jogo máquina vs máquina, os pontos da Maquina2 não estão a ser contabilizados, apesar de esta contagem acentar na mesma função do oponente.

# 5. Demonstração de jogo <a name="p5"></a>

~~~
CL-USER 5 : 1 > (start)

	+------------------------------+
	|                              |
	|  Bem-vindo(a) ao Adji-boto!  |
	|                              |
	| 0 - Sair                     |
	| 1 - Humano vs Computador     |
	| 2 - Computador vs Computador |
	|                              |
	+------------------------------+
	Opcao: 2

	"Profundidade maxima?"
	Opcao: 30

	"Quanto tempo para o computador pensar em ms? (1000-5000)"
	Opcao: 1000

	----------------- NOVO JOGO ----------------

	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (8 8 8 8 8 8) 	     0
	 Maquina2 	 (8 8 8 8 8 8) 	     0
	--------------------------------------------

	 Ronda da maquina 1

	 (0 8 8 8 9 9)
	 (9 9 9 9 9 9)
	 Jogou na posicao: 99
	 Nos analisados: 5333 
	 Numero cortes: 1395 
	 Tempo gasto: 1 
	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (0 8 8 8 9 9) 	     0
	 Maquina2 	 (9 9 9 9 9 9) 	     0
	--------------------------------------------

	 Ronda da maquina 2

	 (0 8 9 9 10 10)
	 (0 10 10 10 10 10)
	 Jogou na posicao: 99
	 Nos analisados: 10775 
	 Numero cortes: 3994 
	 Tempo gasto: 1 
	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (0 8 9 9 10 10) 	     0
	 Maquina2 	 (0 10 10 10 10 10) 	     0
	--------------------------------------------

	 Ronda da maquina 1

	 (1 0 9 9 10 11)
	 (1 11 11 11 11 11)
	 Jogou na posicao: 99
	 Nos analisados: 9788 
	 Numero cortes: 2862 
	 Tempo gasto: 1 
	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (1 0 9 9 10 11) 	     0
	 Maquina2 	 (1 11 11 11 11 11) 	     0
	--------------------------------------------

	 Ronda da maquina 2

	 (1 0 9 9 10 11)
	 (0 12 11 11 11 11)
	 Jogou na posicao: 99
	 Nos analisados: 10825 
	 Numero cortes: 4308 
	 Tempo gasto: 1 
	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (1 0 9 9 10 11) 	     0
	 Maquina2 	 (0 12 11 11 11 11) 	     0
	--------------------------------------------

	 Ronda da maquina 1

	 (0 0 9 9 10 11)
	 (0 12 11 11 11 11)
	 Jogou na posicao: 99
	 Nos analisados: 10941 
	 Numero cortes: 3496 
	 Tempo gasto: 1 
	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (0 0 9 9 10 11) 	     1
	 Maquina2 	 (0 12 11 11 11 11) 	     0
	--------------------------------------------

	 Ronda da maquina 2

	 (1 1 10 10 11 12)
	 (1 1 12 12 12 12)
	 Jogou na posicao: 99
	 Nos analisados: 10391 
	 Numero cortes: 4179 
	 Tempo gasto: 1 
	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (1 1 10 10 11 12) 	     1
	 Maquina2 	 (1 1 12 12 12 12) 	     0
	--------------------------------------------

	 Ronda da maquina 1

	 (0 1 10 10 11 12)
	 (2 1 12 12 12 12)
	 Jogou na posicao: 99
	 Nos analisados: 10692 
	 Numero cortes: 3593 
	 Tempo gasto: 1 
	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (0 1 10 10 11 12) 	     1
	 Maquina2 	 (2 1 12 12 12 12) 	     0
	--------------------------------------------
~~~
## ...
~~~
  Ronda da maquina 1

	 (0 0 0 0 0 0)
	 (0 0 0 0 0 0)
	 Jogou na posicao: 99
	 Nos analisados: 2 
	 Numero cortes: 0 
	 Tempo gasto: 0 
	--------------------------------------------
	 Jogador 	   Tabuleiro 	 Capturadas
	 Maquina1 	 (0 0 0 0 0 0) 	     96
	 Maquina2 	 (0 0 0 0 0 0) 	     0
	--------------------------------------------

	 Maquina2 ganhou o jogo com 96 pecas capturadas contra 0 da maquina1!
~~~