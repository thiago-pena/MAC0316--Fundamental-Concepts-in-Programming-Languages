#lang plai-typed

(define-type ExprC
    [numC   (n : number)]
    [idC    (s : symbol)]
    [appC   (fun : symbol) (arg : ExprC)]
    [plusC  (l : ExprC) (r : ExprC)]
    [multC  (l : ExprC) (r : ExprC)]
    [divC   (l : ExprC) (r : ExprC)]
    [ifC    (condição : ExprC) (sim : ExprC) (não : ExprC)])

(define-type FunDefC
    [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprS
    [numS       (n : number)]
    [idS        (s : symbol)]
    [appS       (fun : symbol) (arg : ExprS)]
    [plusS      (l : ExprS) (r : ExprS)]
    [bminusS    (l : ExprS) (r : ExprS)]
    [uminusS    (e : ExprS)]
    [multS      (l : ExprS) (r : ExprS)]
    [divS       (l : ExprS) (r : ExprS)]
    [ifS        (c : ExprS) (s : ExprS) (n : ExprS)])

(define (desugar [as : ExprS]) : ExprC
    (type-case ExprS as
        [numS       (n)     (numC n)]
        [idS        (s)     (idC s)]
        [appS       (f a)   (appC f (desugar a))]
        [plusS      (l r)   (plusC (desugar l) (desugar r))]
        [multS      (l r)   (multC (desugar l) (desugar r))]
        [divS       (l r)   (divC (desugar l) (desugar r))]
        [bminusS    (l r)   (plusC (desugar l) (multC (numC -1) (desugar r)))]
        [uminusS    (e)     (multC (numC -1) (desugar e))]
        [ifS        (c s n) (ifC (desugar c) (desugar s) (desugar n))]
        ))

(define (interp [a : ExprC] [fds : (listof FunDefC)]) : number
    (type-case ExprC a
        [numC (n) n]
        [idC (_) (error 'interp "shouldn't get here")]
        [appC (f a) (local ([define fd (get-fundef f fds)])
                        (interp (subst a (fdC-arg fd) (fdC-body fd))
                                fds))]
        [plusC (l r) (+ (interp l fds) (interp r fds))]
        [multC (l r) (* (interp l fds) (interp r fds))]
        [divC (l r) (/ (interp l fds) (interp r fds))]
        [ifC (c s n) (if (zero? (interp c fds)) (interp n fds) (interp s fds))]))

#| what: what we want to replace the name with;
 | for: for what name we want to perform substitution;
 | in: in which expression we want to do it.
 |#
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
    (type-case ExprC in
        [numC (n) in]
        [idC (s) (cond
                    [(symbol=? s for) what]
                    [else in])]
        [appC (f a) (appC f (subst what for a))]
        [plusC (l r) (plusC (subst what for l) (subst what for r))]
        [multC (l r) (multC (subst what for l) (subst what for r))]
        [divC (l r) (divC (subst what for l) (subst what for r))]
        [ifC (c s n) (ifC (subst what for c) (subst what for s) (subst what for n))]))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
    (cond
        [(empty? fds) (error 'get-fundef "reference to undefined function")]
        [(cons? fds) (cond
                        [(equal? n (fdC-name (first fds))) (first fds)]
                        [else (get-fundef n (rest fds))])]))

(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(/) (divS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define bib (list
    [fdC 'dobro 'x (plusC (idC 'x) (idC 'x))]
    [fdC 'quadrado 'y (multC (idC 'y) (idC 'y))]
    [fdC 'fatorial 'n (ifC  (idC 'n)
        (multC (appC 'fatorial (plusC (idC 'n) (numC -1)))
            (idC 'n))
            (numC 1))]
    [fdC 'narciso  'narciso (multC (idC 'narciso) (numC 1000))]
    [fdC 'pow2n 'z (ifC  (idC 'z)
     	 (multC (appC 'pow2n (plusC (idC 'z) (numC -1)))
     			(numC 2))
     	 (numC 1))]
     [fdC 'Sn 'm (ifC  (idC 'm)
         	 (plusC (appC 'Sn (plusC (idC 'm) (numC -1)))
         			(idC 'm))
         	 (numC 0))]
    ))


(define (interpS [a : ExprS]) (interp (desugar a) bib))

;(interpS (parse (read)))

; -------------- TESTES --------------
; Este é o arquivo para testes apenas.

; (1) Testes da divisão
(test (interpS (parse '(/ 12 4))) 3)
(test (interpS (parse '(/ (* 4 3) (+ 2 4)))) 2)
(test (interpS (parse '(/ (~ 7) (~ (/ 1  3))))) 21)

; (2) Testes de condicionais
(test (interpS (parse '(if (- 3 2) 42 (+ 8 2)))) 42)
(test (interpS (parse '(if (- 3 3) 42 (+ 8 2)))) 10)
(test (interpS (parse '(if 0 (if 1 2 3) (if 0 4 5)))) 5)

; (3) Testes de chamadas de funções
(test (interpS (parse '(call Sn 100))) 5050)
(test (interpS (parse '(call fatorial 5))) 120)
(test (interpS (parse '(call pow2n 10))) 1024)
(test (interpS (parse '(call dobro 31))) 62)
