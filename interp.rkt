#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [varC  (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [divC  (l : ExprC) (r : ExprC)]
  [andC  (l : ExprC) (r : ExprC)]
  [orC   (l : ExprC) (r : ExprC)]
  [eqC   (l : ExprC) (r : ExprC)]
  [gtC   (l : ExprC) (r : ExprC)]
  [ifC   (condition : ExprC) (yes : ExprC) (no : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [appC (fun : ExprC) (arg : ExprC)]
  [setC (var : symbol) (arg : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)] 
)


(define-type ExprS
  [numS    (n : number)]
  [varS    (s : symbol)] 
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)] 
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [divS    (l : ExprS) (r : ExprS)]
  [andS    (l : ExprS) (r : ExprS)]
  [orS     (l : ExprS) (r : ExprS)]
  [eqS     (l : ExprS) (r : ExprS)]
  [gtS     (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [setS    (var : symbol) (arg : ExprS)]
  [seqS    (b1 : ExprS) (b2 : ExprS)]
)


(define (desugar [as : ExprS]) : ExprC  
  (type-case ExprS as
    [numS    (n) (numC n)]
    [varS    (s) (varC s)]
    [lamS    (a b)  (lamC a (desugar b))]
    [appS    (fun arg) (appC (desugar fun) (desugar arg))] 
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [divS    (l r) (divC (desugar l) (desugar r))]
	  [andS  	 (l r) (andC  (desugar l) (desugar r))]
	  [orS   	 (l r) (orC   (desugar l) (desugar r))]
	  [eqS   	 (l r) (eqC   (desugar l) (desugar r))]
	  [gtS   	 (l r) (gtC   (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c y n) (ifC (desugar c) (desugar y) (desugar n))]
    [setS    (s v)   (setC s (desugar v))]
    [seqS    (b1 b2) (seqC (desugar b1) (desugar b2))]
  )
)

; precisamos de Storage e Locations
(define-type-alias Location number)

; definição do Valor
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
)
  

; associar símbolos a localizações
(define-type Binding
        [bind (name : symbol) (val : Location)]
)

; environment associa FIXME
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Armazenamento
;   bind <-> cell
;   mt-env <-> mt-store
;   extend-env <-> override-store
(define-type Storage
      [cell (location : Location) (val : Value)]
)
(define-type-alias Store (listof Storage))

(define mt-store empty)
(define override-store cons)

; lookup também muda o tipo de retorno
(define (lookup [for : symbol] [env : Env]) : Location
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env))) ; achou!
                                 (bind-val (first env))
                  ]
                  [else (lookup for (rest env))]          ; vê no resto
                  )
            ]
       )
)


; fetch é o lookup do store
(define (fetch [l : Location] [sto : Store]) : Value
       (cond
            [(empty? sto) (error 'fetch "posição não encontrada")]
            [else (cond
                  [(= l   (cell-location (first sto)))  ; achou!
                                (cell-val (first sto))
                  ]
                  [else (fetch l (rest sto))]           ; vê no resto
                  )
            ]
       )
)


;; retorna a próxima localização disponível
(define new-loc
   (let ( [ n (box 0)])
        (lambda () 
           (begin
              (set-box! n (+ 1 (unbox n)))
              (unbox n))
        )
   )
)

; novos operadores
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))
        ]
        [else
             (error 'num+ "Um dos argumentos não é número")
        ]
    )
)

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))
        ]
        [else
             (error 'num* "Um dos argumentos não é número")
        ]
    )
)

(define (num/ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (/ (numV-n l) (numV-n r)))
        ]
        [else
             (error 'num/ "Um dos argumentos não é número")
        ]
    )
)

;; FIXME implementação do AND, OR, EQ e GT

(define-type Result
      [v*s (v : Value) (s : Store)])

;; FIXME que isso??????????
(define (b->n n) (if n 1 0))
(define (n->b n) : boolean (if (zero? n) #f #t))

; Agora o interpretador

(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) n (v*s (numV n) sto)]
    [varC (n)  (v*s (fetch (lookup n env) sto) sto)]  ; busca em cascata, env e em seguida no sto
    [lamC (a b) (v*s (closV a b env) sto)]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1) ; resultado e store retornado por b1
                          (interp b2 env s-b1)
                    ]
                  )
    ]
    ; aplicação de função
    [appC (f a) 
      (type-case Result (interp f env sto) ; acha a função
          [v*s (v-f s-f)
              (type-case Result (interp a env s-f) ; argumento com sto modificado pela função
                 [v*s (v-a s-a)
                      (let ([onde (new-loc)]) ; aloca posição para o valor do argumento
                           (interp (closV-body v-f) ; corpo
                                   (extend-env (bind (closV-arg v-f) onde) ; com novo argumento
                                       (closV-env v-f))
                                   (override-store (cell onde v-a) s-a))) ; com novo valor
                  ]
              )
          ]
      )
    ]
    [plusC (l r) 
            (type-case Result (interp l env sto)
                [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)
                      ]
                    )
                ]
            )
    ]
    [multC (l r) 
            (type-case Result (interp l env sto)
                [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)
                      ]
                    )
                ]
            )
    ]
    [divC (l r) 
            (type-case Result (interp l env sto)
                [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num/ v-l v-r) s-r)
                      ]
                    )
                ]
            )
    ]
    ; FIXME implementação do AND, OR, EQ e GT
    ;[andC (l r)  (b->n (and (n->b (interp l fds)) (n->b (interp r fds))))]
    ;[orC  (l r)  (b->n (or  (n->b (interp l fds)) (n->b (interp r fds))))]
    ;[eqC  (l r)  (b->n (=   (interp l fds) (interp r fds)))]
    ;[gtC  (l r)  (b->n (>   (interp l fds) (interp r fds)))]
    ; ifC já serializa
    [ifC (c y n) (if (zero? (numV-n (v*s-v (interp c env sto)))) (interp n env sto) (interp y env sto))]
  )
)




; o parser permite definir funções
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (varS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(/) (divS  (parse (second sl)) (parse (third sl)))
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(:=) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]
  )
)

; FIXME arrumar facilitador e descobrir se precisa da biblioteca

(define biblioteca (list 
                    [fdC 'porcentagem 'x (divC (idC 'x) (numC 100))]
                    [fdC 'cubo 'y ( multC (multC (idC 'y) (idC 'y)) (idC 'y))]
                    [fdC 'milhasParaKm 'n (multC (numC 1.609) (idC 'n))]
                    [fdC 'fahrentheitParaCelsius 't (plusC (multC (numC 1.8) (idC 't)) (numC 32))]
                    [fdC 'fatorial 'n (ifC (gtC (idC 'n) (numC 1)) 
                                           (multC (appC 'fatorial (plusC (idC 'n) (numC -1))) (idC 'n))
                                           (numC 1))]
                  )
)

(interp (desugar (parse (read))) biblioteca)