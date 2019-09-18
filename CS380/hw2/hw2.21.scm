;Abdullah Aljandali
;CS380 Hw2

#lang eopl
(provide(all-defined-out))


(define-datatype env-type env?
  (empty-env)                    
  (extend-env   [var symbol?]
                [val always?]
                [env env?]))

(define (has-binding? env s)
    (cases env-type env
      [empty-env () #f]
      [extend-env (var val env) (or (equal? var s)
                                    (has-binding? env s))]))
(define (apply-env env s)
  (cases env-type env
    [empty-env () (eopl:error 'apply-env "No binding for ~s" s)] ;if empty, give an error
    [extend-env (var val env) (if (equal? var s)  ; if var = s, return val
                                  val
                                  (apply-env env s))])) ; else, apply it to the rest of env (excluding this pair)
(define (empty-env? env)
  (cases env-type env
    [empty-env () #t]
    [extend-env (var val env) #f]))

;takes a list of vars and a list of values and adds them in order as pairs to the environment
(define (extend-env* var-list val-list env)
   (cond ([null? var-list] env)
        ( #t [extend-env* (cdr var-list) (cdr val-list) (extend-env (car var-list) (car val-list) env)])))



;test 
(define x
  (extend-env 'c '3
              (extend-env 'b '2
                          (extend-env 'a '1
                                      (empty-env)))))

