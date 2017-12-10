#lang racket
(require racket/gui)

; Definicao de Dados e Estruturas
; Estrutura ponto, para desenhar no mapa e realizar ações
(struct ponto (x y))
; Estrutura turno, para revesar os turnos dos jogadores
(struct turno (jogador1 jogador2))
; Estrutura do jogado
(struct player (nome
                posicao ; Posicao dele no mapa
                action ; Guarda a posicao das bombas jogadas
                lvl ; Guarda o nivel dele
                trincheira ; Guarda a posicao da trincheira do jogador
                ))
; Guarda o nomero de jogadores (1 ou 2)
(define num-jogadores 0)
; Definicao dos dois players
(define player1 (player
                 "Player 1"
                 (ponto 0 3)
                 (ponto 0 0)
                 0
                 (ponto 1 0)
                 ))
(define player2 (player
                 "Player 2"
                 (ponto 8 3)
                 (ponto 0 0)
                 0
                 (ponto 7 0)
                 ))

; Definicao do primeiro turno
(define jogada (turno 1 2))
; Guarda as posicoes das explosoes
(define explosoes empty)

; Imagens utilizadas
(define tela-inicial (read-bitmap "./bg2.png")) ; Background da tela inicial
(define grass (read-bitmap "./grassGrid2.png")) ; Grama da tela de jogo
(define trincheira (read-bitmap "./arame2.png")) ; Trincheira
(define bandeira (read-bitmap "./doende.png")) ; Bandeira
(define plr1 (read-bitmap "./plr1.png")) ; Imagem Player 1
(define plr2 (read-bitmap "./plr2.png")) ; Imagem Player 2
(define boom (read-bitmap "./boom.png")) ; Imagem explosão

; ========================================================= Frames
; Definicao da Tela Inicial (frame)
(define frame-inicial (new frame%
                   [label "Trench Fight"] ; Nome que aparece em cima da janela
                   [width 450] ; Largura
                   [height 379])) ; Altura

(new canvas% [parent frame-inicial]
             [paint-callback
              (lambda (canvas dc)
                (send dc draw-bitmap tela-inicial 0 0) ; Desenha o background
              )]
             )

(new button%
     [parent frame-inicial]
     [label "Começar!"]
     [callback (λ (button event) (send dialog show #t))])

; Definicao da Tela de Jogo (frame)
(define frame-jogo (new frame%
                   [label "Action time!"]
                   [width 450]
                   [height 379]))

;(define canvas-jogo (new canvas-jogo% [parent frame-jogo]
(define canvas-jogo (new canvas% [parent frame-jogo]
             [paint-callback
              (lambda (canvas dc)
                (setup-play)
              )]
             ))
(new button%
     [parent frame-jogo]
     [label "Play!"]
     [callback
      (λ (button event)
        (role-play)
        )
      ]
     )
(define dc-jogo (send canvas-jogo get-dc))



; ========================================== Telas de Inicio ====================
; Cria caixa de selecao do numero de jogadores
(define (botao-num-players num)
  (new button%
     [parent panel]
     [label (number->string num)]
     [callback (λ (button event)
                 (set! num-jogadores num)
                 (send dialog show #f)
                 (seleciona-nomes))])
  )

; Cria caixa que pede o numero de jogadores
(define dialog (instantiate dialog% ("Número de Jogadores")
                 [width 300]))
(new message% [parent dialog] [label "Escolha o número de jogadores!"])
(define panel (new horizontal-panel% [parent dialog]
                                     [alignment '(center center)]))

(botao-num-players 1)
(botao-num-players 2)

(when (system-position-ok-before-cancel?)
  (send panel change-children reverse))



; Realiza a seleção do nome dos jogadores
(define (botao-nomes num pai)
   (new text-field% [parent pai]
       [label "Insira o nome:"]
       [init-value num])
  )

; Janela para 1 nome
(define (janela-nome1 dialog-nomes)
  (define botao1 (botao-nomes "Player 1" dialog-nomes))

   (define panel-nomes (new horizontal-panel% [parent dialog-nomes]
                     [alignment '(center center)]))

  (new button%
       [parent panel-nomes]
       [label "Confirmar!"]
       [callback (λ (button event)
                   (set! player1
                         (player
                          (send botao1 get-value)
                          (player-posicao player1)
                          (player-action player1)
                          (player-lvl player1)
                          ))
                   (send dialog-nomes show #f)
                   (begin-action)
                   )])
  )

; Janela para 2 nomes
(define (janela-nome2 dialog-nomes)
  (define botao1 (botao-nomes "Player 1" dialog-nomes))
  (define botao2 (botao-nomes "Player 2" dialog-nomes))
  
  (define panel-nomes (new horizontal-panel% [parent dialog-nomes]
                           [alignment '(center center)]))
  
  (new button%
       [parent panel-nomes]
       [label "Confirmar!"]
       [callback (λ (button event)
                   (set! player1
                         (player
                          (send botao1 get-value)
                          (player-posicao player1)
                          (player-action player1)
                          (player-lvl player1)
                          (player-trincheira player1)
                          ))
                  (set! player2
                         (player
                          (send botao2 get-value)
                          (player-posicao player2)
                          (player-action player2)
                          (player-lvl player2)
                          (player-trincheira player2)
                          ))
                   (send dialog-nomes show #f)
                   (begin-action)
                   )])
  )

; Chama as janelas
(define (seleciona-nomes)
  ;Cria caixa para a selecao de nomes
  (define dialog-nomes (instantiate dialog% ("Nome dos Jogadores")
                         [width 300]
                         ))
  
  (cond
    [(= num-jogadores 1) (janela-nome1 dialog-nomes)]
    [else (janela-nome2 dialog-nomes)]
    )

  (send dialog-nomes show #t)
  )

; ============================================================================
; Fecha a tela inicial e mostra a tela do jogo
(define (begin-action)
  (send frame-inicial show #f)
  (send frame-jogo show #t)
  )

; Desenha a posicao dos players, trincheiras e o nome dos players
(define (setup-play)
  (send dc-jogo draw-bitmap grass 0 0)
  (desenha-bandeiras)
  (posicao-inicial-players)
  (posiciona-trincheira (player-trincheira player1))
  (posiciona-trincheira (player-trincheira player2))
  (desenha-nome-players)
  )

; Acao do jogo
(define (role-play)
  (define jogador1 (turno-jogador1 jogada))
  (define jogador2 (turno-jogador2 jogada))
  
  (movimento jogador1 jogador2) ; Pede movimento dos dois jogadores
  (cond
    [(check-acerto? jogador1 jogador2) ; Confere se o jogador acertou o outro
     (move-trench jogador1) ; Caso sim, anda com a trincheira
     (printf "Acerto!")
     ]
    [else
     (printf "Errou!")
     ]
    )
  ; Fecha o frame do jogo, atualiza e abre novamente
  (send frame-jogo show #f)
  (setup-play)
  (cond [(= jogador1 1)
         (posiciona-desenho boom (player-action player1))]
        [else (posiciona-desenho boom (player-action player2))])
  
  (set! jogada (turno (turno-jogador2 jogada) (turno-jogador1 jogada)))
  (send frame-jogo show #t)
  
  )


; ============================================================= Desenhar
; Desenha o nome dos players nas laterais
(define (desenha-nome-players)
  (send dc-jogo set-scale 1 1)
  (send dc-jogo set-text-foreground "white")
  (send dc-jogo draw-text (player-nome player1) 5 345 #t 0 1.57)
  (send dc-jogo set-scale 1 1)
  (send dc-jogo set-text-foreground "white")
  (send dc-jogo draw-text (player-nome player2) 425 345 #t 0 1.57))

; Funcao que desenha um trincheira
(define (posiciona-trincheira ponto)
    (send dc-jogo draw-bitmap trincheira (* (ponto-x ponto) 50) (* (ponto-y ponto) 50))
  )

; Funcao generica de desenho em um ponto
(define (posiciona-desenho desenho ponto)
  (send dc-jogo draw-bitmap desenho (* (ponto-x ponto) 50) (* (ponto-y ponto) 50)))

; Desenha os players
(define (posicao-inicial-players)
  (posiciona-desenho plr1 (player-posicao player1))
  (posiciona-desenho plr2 (player-posicao player2))
  )

; Desenha a bandeira/Doende
(define (desenha-bandeiras)
  ;(posiciona-desenho bandeira (ponto 4 0))
  ;(posiciona-desenho bandeira (ponto 4 2))
  ;(posiciona-desenho bandeira (ponto 4 4))
  ;(posiciona-desenho bandeira (ponto 4 6))
  (posiciona-desenho bandeira (ponto 4 3))
  )

; ============================================================= Movimento
; Pega a cordenada passada pelos jogadores
(define (campos-coordenadas pai texto)
  (new text-field% [parent pai]
       [label texto]
       [init-value "0"])
  )
; Realiza um update da posicao no jogador
(define (change-posicao jogador coluna2 linha2) 
  (cond
    [(= jogador 1)
     (set! player1
           (player
            (player-nome player1)
            (ponto (string->number (send linha2 get-value)) (string->number (send coluna2 get-value)))
            (player-action player1)
            (player-lvl player1)
            (player-trincheira player1)
            ))]
    [else
     (set! player2
           (player
            (player-nome player2)
            (ponto (string->number (send linha2 get-value)) (string->number (send coluna2 get-value)))
            (player-action player2)
            (player-lvl player2)
            (player-trincheira player2)
            ))
     ]
    )
  )
; Realiza update na ação do jogador
(define (change-action jogador coluna1 linha1)
  (cond
    [(= jogador 1)
     (set! player1
           (player
            (player-nome player1)
            (player-posicao player1)
            (ponto (string->number (send linha1 get-value)) (string->number (send coluna1 get-value)))
            (player-lvl player1)
            (player-trincheira player1)
            ))]
    [else
     (set! player2
           (player
            (player-nome player2)
            (player-posicao player2)
            (ponto (string->number (send linha1 get-value)) (string->number (send coluna1 get-value)))
            (player-lvl player2)
            (player-trincheira player2)
            ))
     ]
    )
  )
(define (valid-esquiva? player coluna)
  (define column (string->number (send coluna get-value)))
  (cond
    [(= player 1)
     (cond
       [(> (ponto-x (player-trincheira player1)) column)
        #t]
       [else #f])]
    [else
     (cond
       [(< (ponto-x (player-trincheira player2)) column)
        #t]
       [else #f])]
    )
  )

; Funcao que leva o jogador a escolher um lugar para esquivar-se
(define (action-correr num)
  (define dialog-correr (instantiate dialog% ("Correr")
                          [width 200]
                          ))
  (define nome "")
  (cond
    [(= num 1) (set! nome (player-nome player1))]
    [else (set! nome (player-nome player2))]
    )
  
  (new message% [parent dialog-correr] [label nome])
  (new message% [parent dialog-correr] [label "Sua vez de correr!"])
  (define linha2 (campos-coordenadas dialog-correr "Insira uma linha"))
  (define coluna2 (campos-coordenadas dialog-correr "Insira uma coluna"))
  (define panel-correr (new horizontal-panel% [parent dialog-correr]
                            [alignment '(center center)]))
  (new button%
       [parent panel-correr]
       [label "Confirmar"]
       [callback (λ (button event)
                   (cond
                     [(valid-esquiva? num coluna2)
                      (change-posicao num linha2 coluna2)
                      (send dialog-correr show #f)
                      ]
                     [else
                      (send dialog-correr show #f)
                      (send dialog-correr show #t)
                      ]
                     
                     ))])
  
  
  (send dialog-correr show #t)
  )
; Funcao que leva o jogador a escolher um lugar para atacar
(define (action-atacar num)
  (define dialog-atacar (instantiate dialog% ("Atacar")
                          [width 200]
                          ))
  
  (define nome "")
  (cond
    [(= num 1) (set! nome (player-nome player1))]
    [else (set! nome (player-nome player2))]
    )
  (new message% [parent dialog-atacar] [label nome])
  (new message% [parent dialog-atacar] [label "Sua vez de atacar!"])
  (define linha1 (campos-coordenadas dialog-atacar "Insira uma linha"))
  (define coluna1 (campos-coordenadas dialog-atacar "Insira uma coluna"))
  (define panel-atacar (new horizontal-panel% [parent dialog-atacar]
                            [alignment '(center center)]))
  (new button%
       [parent panel-atacar]
       [label "Confirmar"]
       [callback (λ (button event)
                   (change-action num linha1 coluna1)
                   (send dialog-atacar show #f)
                   )])
  
  (send dialog-atacar show #t)
  )

; Chama as duas funcoes de atacar/correr
(define (movimento jogador1 jogador2)
  (action-atacar jogador1)
  (action-correr jogador2)
  )

; =========================================================== Ações colisao
; Verifica se o jogador 1 acertou o outro
(define (check-acerto? jogador1 jogador2)
  (define atacante empty)
  (define defensor empty)

  (cond [(= jogador1 1)
         (set! atacante player1)
         (set! defensor player2)
         ]
        [else
         (set! atacante player2)
         (set! defensor player1)]
        )
  
  (cond [(and
          (= (ponto-x (player-action atacante)) (ponto-x (player-posicao defensor)))
          (= (ponto-y (player-action atacante)) (ponto-y (player-posicao defensor))))
         #t]
      [else
       #f]
      )
  )

; Move a trincheira uma posicao pra frente
(define (move-trench jogAux)
  (define jogador empty)
  (cond [(= jogAux 1)
         (set! jogador player1)]
        [else (set! jogador player2)])
  
  (cond
    [(= jogAux 1)
     (set! player1
                         (player
                          (player-nome player1)
                          (player-posicao player1)
                          (player-action player1)
                          (player-lvl player1)
                          (ponto (add1 (ponto-x (player-trincheira player1))) (ponto-y (player-trincheira player1)))
                          ))]
    [else
     (set! player2
                         (player
                          (player-nome player2)
                          (player-posicao player2)
                          (player-action player2)
                          (player-lvl player2)
                          (ponto (sub1 (ponto-x (player-trincheira player2))) (ponto-y (player-trincheira player2)))
                          ))]
    )
  )

; =========================================================== Inicio
(define (inicio)
  (send frame-inicial show #t)
  )

(inicio)