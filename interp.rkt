#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC  (s : symbol)]  
  [appC (fun : symbol) (arg : ExprC)] 
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [divC  (l : ExprC) (r : ExprC)]
  [andC  (l : ExprC) (r : ExprC)]
  [orC   (l : ExprC) (r : ExprC)]
  [eqC   (l : ExprC) (r : ExprC)]
  [gtC   (l : ExprC) (r : ExprC)]
  [ifC   (condition : ExprC) (yes : ExprC) (no : ExprC)]
)

; definição de função com 1 argumento
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)]
)


; inclui funções
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)] 
  [appS    (fun : symbol) (arg : ExprS)] 
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
)

; agora é preciso arrumar desugar interpretador e parser.

(define (desugar [as : ExprS]) : ExprC  
  (type-case ExprS as
    [numS    (n) (numC n)]
    [idS     (s) (idC s)] 
    [appS    (fun arg) (appC fun (desugar arg))] ; fun é um symbol, não precisa de desugar 
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
  )
)


; subst substitui THIS por VALUE em EM
(define (subst [value : ExprC] [this : symbol] [em : ExprC]) : ExprC
  (type-case ExprC em
    [numC (n) em]   ; nada a substituir, repassa
    [idC (s) (cond  ; poderia ser 'if', mas existem coisas no futuro...
               [(symbol=? s this) value] ; símbolo, troque
               [else em])] ; deixa quieto
    [appC  (f a) (appC f (subst value this a))] ; chamada de função - arruma o argumento
    [plusC (l r) (plusC (subst value this l) (subst value this r))]
    [multC (l r) (multC (subst value this l) (subst value this r))]
    [divC  (l r) (divC  (subst value this l) (subst value this r))]
    [andC  (l r) (andC  (subst value this l) (subst value this r))]
    [orC   (l r) (orC   (subst value this l) (subst value this r))]
    [eqC   (l r) (eqC   (subst value this l) (subst value this r))]
    [gtC   (l r) (gtC   (subst value this l) (subst value this r))]
    [ifC (c y n) (ifC   (subst value this c) (subst value this y) (subst value this n))]
  )
)

; Agora o interpretador!
(define (b->n n) (if n 1 0))
(define (n->b n) : boolean (if (zero? n) #f #t))

(define (interp [a : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    ; Aplicação de função é que precisa de subst
    [appC (f a) 
          (local ([define fd (get-fundef f fds)]) ; pega a definição em fd
            (interp (subst a                 ; interpreta o resultado de subst
                           (fdC-arg fd)
                           (fdC-body fd)
                           )
                    fds))]
    ; Não devem sobrar idenficadores livres na expressão
    [idC (_) (error 'interp "não deveria encontrar isso!")]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [divC (l r)  (/ (interp l fds) (interp r fds))]
    [andC (l r)  (b->n (and (n->b (interp l fds)) (n->b (interp r fds))))]
    [orC  (l r)  (b->n (or  (n->b (interp l fds)) (n->b (interp r fds))))]
    [eqC  (l r)  (b->n (=   (interp l fds) (interp r fds)))]
    [gtC  (l r)  (b->n (>   (interp l fds) (interp r fds)))]
    [ifC (c y n) (if (zero? (interp c fds)) (interp n fds) (interp y fds))]
  )
)

; get-fundef
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "referência para função não definida")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)] ; achou!
                   [else (get-fundef n (rest fds))] ; procura no resto
                  )
    ]
  )
)


; o parser precisa tratar tanto de chamadas
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(/) (divS  (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]
  )
)


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