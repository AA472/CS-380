
(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 
  
  (require "data-structures.scm")
  
  (provide init-env empty-env extend-env extend-env* extend-rec-env apply-env)

  ;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define (init-env)
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env)))))

  ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define (empty-env)
    (empty-env-record))
  
  (define (empty-env? x)
    (cases environment x
      [empty-env-record () #t]
      [else #f]))
  
  (define (extend-env sym val old-env)
    (extended-env-record sym val old-env))

    (define (extend-env* var-list val-list env)
   (cond ([null? var-list] env)
        ( #t [extend-env* (cdr var-list) (cdr val-list)
                          (extend-env (car var-list) (car val-list) env)])))
  
  (define (extend-rec-env proc-name arg proc-body env)
    (extended-rec-env proc-name arg proc-body env))
  


  (define (apply-env env search-sym)
    (cases environment env
      [empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extended-env-record (sym val old-env)
                           (if (eqv? search-sym sym)
                               val
                               (apply-env old-env search-sym))]
      [extended-rec-env (proc-name arg proc-body old-env) (let loop ([proc-name proc-name]
                                                                     [arg arg]
                                                                     [proc-body proc-body])
                                                            (if (null? proc-name)
                                                                (apply-env old-env search-sym)
                                                                (if (eqv? search-sym (car proc-name))
                                                                    (proc-val (procedure (car arg)
                                                                                         (car proc-body)
                                                                                         env))
                                                                    (loop  (cdr proc-name) (cdr arg) (cdr proc-body)))))]))

                
  
  )