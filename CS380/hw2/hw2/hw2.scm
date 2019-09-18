;Abdullah Aljandali
;CS380 Hw2


#lang eopl
(provide(all-defined-out))

; Exercise 2.50

(define (empty-env)  ;empty environment
  '() )

;extend environment adds a variable and a value pair to the environment
(define (extend-env var val env)
  (append env  (list (cons var val))))


; Exercise 2.8

;empty-env? checks if the environment is empty and returns a bolean
(define (empty-env? env)
  (if (equal? env '() ) #t #f))

; Exercise 2.9

;checks if a variable has a binding in the environment and returns a bolean
(define (has-binding? s env)
  (cond ([empty-env? env] #f)
        ([equal? s (caar env)] #t )
        ( #t [has-binding? s (cdr env)])))

; Exercise 2.10

;takes a list of vars and a list of values and adds them in order as pairs to the environment
(define (extend-env* var-list val-list env)
   (cond ([null? var-list] env)
        ( #t [extend-env* (cdr var-list) (cdr val-list) (extend-env (car var-list) (car val-list) env)])))

; Exercise 2.21

;implements environment as a datatype
(define-datatype env1-type env1?
  (empty-env1)
  (extend-env1 [var symbol?]
               [val always?]
               [env1 env1?]))


(define (has-binding1? env1 s)
    (cases env1-type env1
      [empty-env1 () #f]
      [extend-env1 (var val env1) (or (equal? var s)
                                    (has-binding1? env1 s))]))

;test 
(define x
  (extend-env 'c '3
              (extend-env 'b '2
                          (extend-env 'a '1
                                      (empty-env)))))

