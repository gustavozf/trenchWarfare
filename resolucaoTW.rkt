#lang racket

(struct ponto (x y))
; Estrutura turno, para revesar os turnos dos jogadores
(struct turno (jogador1 jogador2))
; Estrutura do jogado
(struct player (nome
                ;posicao ; Posicao dele no mapa
                ;action ; Guarda a posicao das bombas jogadas
                lvl ; Guarda o nivel dele
                trincheira ; Guarda a posicao da trincheira do jogador
                ))
; Guarda o nomero de jogadores (1 ou 2)
(define num-jogadores 0)
; Definicao dos dois players
(define player1 (player
                 "Player 1"
                 ;(ponto 0 3)
                 ;(ponto 0 0)
                 1
                 (ponto 1 0)
                 ))
(define player2 (player
                 "Player 2"
                 ;(ponto 8 3)
                 ;(ponto 0 0)
                 1
                 (ponto 7 0)
                 ))

; Definicao do primeiro turno
(define jogada (turno 1 2))
; Numero de turnos realizados
(define num-turnos 0)
; Ultimas jogadas realizadas
(define jogadas empty)

(define movimentos-possiveis1 (list
                               (ponto 0 0)
                               (ponto 1 0)
                               (ponto 2 0)
                               (ponto 3 0)
                               (ponto 4 0)
                               (ponto 5 0)
                               ))
(define movimentos-possiveis2 (list
                               (ponto 0 8)
                               (ponto 1 8)
                               (ponto 2 8)
                               (ponto 3 8)
                               (ponto 4 8)
                               (ponto 5 8)
                               ))



; Acoes do player 2 -> Totalmente randomicas
(define (random-element list)
  (list-ref list (random (length list))))

(define (player1-atacar)
  (random-element movimentos-possiveis2))

(define (player2-atacar)
  (random-element movimentos-possiveis1))

(define (player1-correr)
  (random-element movimentos-possiveis1))

(define (player2-correr)
  (random-element movimentos-possiveis2))

(define (hit? posicao1 posicao2)
  (cond
    [(and (= (ponto-x posicao1) (ponto-x posicao2))
          (= (ponto-y posicao1) (ponto-y posicao2)))
             #t]
    [else #f]
    )
  )

(define (jogo)
  (cond
    [(hit? (player2-correr) (player1-atacar))
     (set! player1 (player
                    (player-nome player1)
                    (add1 (player-lvl player1))
                    (player-trincheira player1)))])
  (cond
    [(= (player-lvl player1) 4) (printf "Player1 Ganhou!\n")]
    [else
     (cond
       [(hit? (player1-correr) (player2-atacar))
        (set! player2 (player
                       (player-nome player2)
                       (add1 (player-lvl player2))
                       (player-trincheira player2)))])
     (cond
       [(= (player-lvl player2) 4) (printf "Player2 Ganhou!\n")]
       [else
        (jogo)])]))

(jogo)