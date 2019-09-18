;Abdullah Aljandali
;CS380 Hw2

#lang eopl
(provide(all-defined-out))

; Exercise 2.5

(define (empty-env)  ;empty environment
  '() )

;checks if s is a var in env
;if so, it returns the associated val
;else it returns an error
(define (apply-env env s)
  (cond ([empty-env? env] (eopl:error 'apply-env "No binding for ~s" s) )
        ([equal? s (caar env)] [cdar env])
        (#t [apply-env (cdr env) s])))


;extend environment adds a variable and a value pair to the environment
(define (extend-env var val env)
 ; (append env (cons (cons var val) '() )))
  (cons ( cons var val ) env))


; Exercise 2.8

;empty-env? checks if the environment is empty and returns a bolean
(define (empty-env? env)
  (if (equal? env '() ) #t #f))

; Exercise 2.9

;checks if a variable has a binding in the environment and returns a bolean
(define (has-binding? env s)
  (cond ([empty-env? env] #f)
        ([equal? s (caar env)] #t )
        ( #t [has-binding? (cdr env) s])))

; Exercise 2.10

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

