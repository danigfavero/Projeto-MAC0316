#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [divC (l : ExprC) (r : ExprC)]
  [ifC   (condition : ExprC) (yes : ExprC) (no : ExprC)]
  ; Implementação das funções:
  [idC  (s : symbol)]
  [lamC (arg : symbol) (body : ExprC)]
  [appC (fun : ExprC) (arg : ExprC)] 
)

(define-type ExprS
  [numS    (n : number)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [divS    (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [idS     (s : symbol)] 
  [lamS    (arg : symbol) (body : ExprS)] 
  [appS    (fun : ExprS) (arg : ExprS)]
)


(define (desugar [as : ExprS]) : ExprC  
  (type-case ExprS as
    [numS    (n)   (numC n)]
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [divS    (l r) (divC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c y n) (ifC (desugar c) (desugar y) (desugar n))]
    [idS     (s) (idC s)]
    [lamS    (a b)  (lamC a (desugar b))]
    [appS    (fun arg) (appC (desugar fun) (desugar arg))] 
  )
)


; Environment
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
)

; Símbolos devem se associar ao número
(define-type Binding
  [bind (name : symbol) (val : Value)]
)

; A lista de associações é o environment
(define-type-alias Env (listof Binding))
(define mt-env empty)        ; cria o env vazio
(define extend-env cons)     ; estende o env


; Operadores
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
      (numV (+ (numV-n l) (numV-n r)))
    ]
    [else
      (error 'num+ "Um dos argumentos não é número")]
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


; O interpretador
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [lamC (a b) (closV a b env)] ; definição de função captura o environment

    [appC (f a)
      (local ([define f-value (interp f env)]) ; f-value descreve melhor a ideia
        (interp (closV-body f-value)
          (extend-env 
            (bind (closV-arg f-value) (interp a env))
            (closV-env f-value) ; não mais mt-env
          )
        )
      )
    ]

    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [divC (l r)  (num/ (interp l env) (interp r env))]
    [ifC (c y n) (if (zero? (numV-n (interp c env))) (interp n env) (interp y env))]
  )
)

; função para buscar valores no environment
(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
    [else (cond
          [(symbol=? for (bind-name (first env)))   ; achou!
                         (bind-val (first env))
          ]
          [else (lookup for (rest env))] ; vê no resto
          )
    ]
  )
)       


(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
      (let ([sl (s-exp->list s)])
        (case (s-exp->symbol (first sl))
          [(+) (plusS (parse (second sl)) (parse (third sl)))]
          [(*) (multS (parse (second sl)) (parse (third sl)))]
          [(/) (divS  (parse (second sl)) (parse (third sl)))]
          [(-) (bminusS (parse (second sl)) (parse (third sl)))]
          [(~) (uminusS (parse (second sl)))]
          [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
          [(call) (appS (parse (second sl)) (parse (third sl)))]
          [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
          [else (error 'parse "invalid list input")]
        )
      )
    ]
    [else (error 'parse "invalid input")]
  )
)

; Facilitador
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))
