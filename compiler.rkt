#lang typed/racket

(require typed/rackunit)


; AST as a set of Expressions
(define-type ExprM (U Real Symbol LamM PrimM AppM IfM))
(struct LamM ([arg : (Listof Symbol)] [body : ExprM]) #:transparent)
(struct PrimM ([opr : Symbol] [args : (Listof ExprM)]) #:transparent)
(struct AppM ([fun : ExprM] [arg : ExprM]) #:transparent)
(struct IfM ([check : ExprM] [then : ExprM] [else : ExprM]) #:transparent)


; Parses lambda calculus into AST
(define (parse [s : Sexp]) : ExprM
  (match s
    [(or (? real?) (? symbol?)) s]
    [`(ifleq0 ,check ,then ,else) (IfM (parse check) (parse then) (parse else))]
    [`(/ ,(and args (list (? symbol?) ...)) => ,body) (LamM (cast args (Listof Symbol)) (parse body))]
    [`(,(and opr (or '+ '* 'println)) ,args ...) (PrimM opr (map parse (cast args (Listof Sexp))))]
    [`(,fun ,arg) (AppM (parse fun) (parse arg))]
    [_ (error 'parse (string-append "Unrecognized expression: " (~v s)))]))


; translates AST into EMCA6 JS String
(define (translate-js [e : ExprM]) : String
  (match e
    [(or (? real?) (? symbol?)) (~a e)]
    [(PrimM (and opr (or '* '+)) `(,left ,right)) (format "(~a ~a ~a)" (translate-js left) (translate-js opr) (translate-js right))]
    [(PrimM 'println `(,val)) (format "console.log(~a)" (translate-js val))]
    [(AppM fun arg) (format "~a(~a)" (translate-js fun) (translate-js arg))]
    [(IfM check then else) (format "((~a <= 0) ? ~a : ~a)" (translate-js check) (translate-js then) (translate-js else))]
    [(LamM args body) (format "((~a) => ~a)" (string-join (map ~a args) ", ") (translate-js body))]))


(define (lambda-js-translate [s : Sexp]) : String
  (translate-js (parse s)))


 ;-----TESTS-----
(check-equal? (parse '(ifleq0 0 2 3)) (IfM 0 2 3))
(check-equal? (parse '(/ (x y) => (+ x y))) (LamM '(x y) (PrimM '+ '(x y))))
(check-equal? (parse '(println (* 2 3))) (PrimM 'println (list (PrimM '* '(2 3)))))
(check-equal? (parse '((/ (f) => (+ (f 1) (f 2)))(/ (x) => (* x x))))
              (AppM (LamM '(f) (PrimM '+ (list (AppM 'f 1) (AppM 'f 2)))) (LamM '(x) (PrimM '* '(x x)))))


(check-equal? (translate-js (IfM 0 2 3)) "((0 <= 0) ? 2 : 3)")
(check-equal? (translate-js (LamM '(x y) (PrimM '+ '(x y)))) "((x, y) => (x + y))")
(check-equal? (translate-js (PrimM 'println (list (PrimM '* '(2 3))))) "console.log((2 * 3))")
(check-equal? (translate-js (AppM (LamM '(f) (PrimM '+ (list (AppM 'f 1) (AppM 'f 2)))) (LamM '(x) (PrimM '* '(x x)))))
              "((f) => (f(1) + f(2)))(((x) => (x * x)))")