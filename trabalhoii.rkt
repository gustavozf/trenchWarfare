 #lang racket/gui
(require racket/gui)
(require racket/draw)
(require racket/mpair)

(struct ponto (x y))
(define-values (tabuleiro) (list (list 11 12 13 14 15 16 17)
                                 (list 21 22 23 24 25 26 27)
                                 (list 31 32 33 34 35 36 37)
                                 (list 41 42 43 44 45 46 47)
                                 (list 51 52 53 54 55 56 57)
                                 (list 61 62 63 64 65 66 67)
                                 (list 71 72 73 74 75 76 77)
                                 (list 81 82 83 84 85 86 87)
                                 (list 91 92 93 94 95 96 97)
                                 ))

(define (print-tabuleiro z)
  (define (print-linha i)
    (cond 
           [(not(empty? i))
             (print (first i))
             (printf " ")
             (print-linha (rest i))
            ]
           [else
            (printf "\n")]
           )
    )

  (define (print-todo j)
    (cond
      [(empty? j) (printf "\n")]
      [else
       (print-linha (first j))
       (print-todo (rest j))
       ]
     )
    )
  (print-todo z)
  )

(define (iniciacao)
  (printf "TRENCHES FIGHT\n\n")
  (printf "A real experiência da guerra entre\n")
  (printf "trincheiras, basado na WWI!\n\n")
  (printf "Escolha o número de jogadores(1-2):")
  )

(define (inicio)
  (iniciacao)
  (define-values (num) (read (current-input-port)))
  (printf "Insira seu nome: ")
  (define-values (in) (read (current-input-port)))
  (print in)
  (printf "\n")
  
  (print-tabuleiro tabuleiro)
  )

(inicio)