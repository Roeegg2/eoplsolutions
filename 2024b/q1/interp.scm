(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (enum-exp (ids)
          (if (null? ids)
            (eopl:error 'enum-exp "Cannot have empty enum")
            (enum-val ids)))

        (enum-elmt-exp (enm id)
          (check-id (expval->enum (value-of enm env)) id))


        (match-exp (enmid exp1 enmids exps)
          (let ((enm (expval->enum (apply-env env enmid)))
                (id (expval->id (value-of exp1 env))))
                  (begin
                    (check-match enm enmids exps)
                    (value-of (do-match enmids exps id) env))))  ; Evaluate the matched expression

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of body (extend-env var val saved-env))))))

  (define check-id
    (lambda (ids id)
      (if (null? ids)
        (eopl:error 'check-id "No such variant")
        (if (eqv? (car ids) id)
          (id-val id)
          (check-id (cdr ids) id)))))

  (define check-match
    (lambda (orgids ids exps)
      (cond 
        ((and (and (null? ids) (null? orgids)) (null? exps)) '())
        ((null? ids) (eopl:error "Malformed: missing ids"))
        ((null? exps) (eopl:error "Malformed: missing exps"))
        ((null? orgids) (eopl:error "Malformed: invalid ids listed"))
        ((not (eqv? (car orgids) (car ids))) (eopl:error "Unknown variant listed"))
        (else (check-match (cdr orgids) (cdr ids) (cdr exps))))))

  (define do-match
    (lambda (ids exps id)
      (if (or (null? ids) (null? exps))
        (eopl:error "Invalid variant")
        (if (eqv? (car ids) id)
          (car exps)
            (do-match (cdr ids) (cdr exps) id)))))
  





























  ; (define check-match
  ; (lambda (orgids ids exps)
  ;   (cond
  ;     ;; If we've processed all pattern IDs, check if we have the right number of expressions
  ;     ((null? ids) 
  ;      (if (null? exps) 
  ;          #t  ; Success - all patterns matched
  ;          (eopl:error 'check-match "Malformed: extra expressions")))
  ;
  ;     ;; If we have pattern IDs but no expressions, error
  ;     ((null? exps) 
  ;      (eopl:error 'check-match "Malformed: missing expressions"))
  ;
  ;     ;; Check if the current pattern ID is valid (exists in original enum)
  ;     ((not (member (car ids) orgids))
  ;      (eopl:error 'check-match "Unknown ID in match"))
  ;
  ;     ;; Recursively check the rest
  ;     (else 
  ;      (check-match orgids (cdr ids) (cdr exps))))))

  ; (define check-match
  ;   (lambda (orgids ids exps)
  ;     (if (null? orgids)
  ;       (eopl:error 'check-match "Too many variants")
  ;       (if (null? ids)
  ;         (if (null? exps)
  ;           (eopl:error "No such id")
  ;           (eopl:error "Malformed: missing ident"))
  ;         (if (null? exps)
  ;           (eopl:error "Malformed: missing exps")
  ;           (if (eqv? (car ids) (car orgids))
  ;             (check-match (cdr orgids) (cdr ids) (cdr exps))
  ;             (eopl:error "Unknown ID in match")))))))


  ; (define do-match
  ; (lambda (ids exps id)
  ;   (begin
  ;   (eopl:printf "HERE\n")
  ;   (cond
  ;     ;; If no more patterns, this shouldn't happen if check-match worked
  ;     ((null? ids) 
  ;      (eopl:error 'do-match "No matching pattern found"))
  ;
  ;     ;; If we found the matching pattern ID, return the corresponding expression
  ;     ((eqv? (car ids) id) 
  ;      (car exps))
  ;
  ;     ;; Otherwise, keep searching
  ;     (else 
  ;      (do-match (cdr ids) (cdr exps) id))))))
        
  ; (define do-match
  ;   (lambda (ids exps id)
  ;     (if (eqv? (car ids) id)
  ;       (car exps)
  ;       (do-match (cdr ids) (cdr exps) id))))






  )
