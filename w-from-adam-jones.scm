;; Code based on Adam Jones' implementation of Hindley-Milner
;; type inference algorithm that can be found at
;; https://github.com/domdomegg/hindley-milner-typescript.git
;; with video tutorial available at
;; https://www.youtube.com/@adam-jones/videos

(use-modules (grand scheme))

;; We represent type variables as two-element lists,
;; whose first element is the symbol `unquote'.
;; This allows us to write examples in a very concise
;; manner, but it might be confusing to beginner
;; Lisp programmers, because the code also uses
;; `unquote' in the regular way (i.e. inside `quasiquote')
;;

(define type-variable-marker 'unquote)

(define (type-variable? x)
  (and-let* ((`(,,type-variable-marker ,_) x))))

(e.g.
 (type-variable? ',x))

(e.g.
 (isnt 'x type-variable?))

(define (type-variable-name type-variable)
  (match type-variable
    (`(,,type-variable-marker ,name)
     name)))

(e.g.
 (type-variable-name ',x) ===> x)

(define (bound? variable #;in bindings)
  (match bindings
    (`((,,variable . ,_) . ,_) #t)
    (`(,_ . ,sequel) (bound? variable #;in sequel))
    ('() #f)))

(e.g.
 (bound? ',b '((,a . 1) (,b . 2) (,c . 3))))

(e.g.
 (isnt 'b bound? #;in '((,a . 1) (,b . 2) (,c . 3))))

(define (lookup variable bindings)
  (match bindings
    (`((,,variable . ,value) . ,_) value)
    (`((,_ . ,_) . ,sequel) (lookup variable sequel))))

(e.g.
 (lookup ',b '((,a . 1) (,b . 2) (,c . 3))) ===> 2)

(e.g.
 (lookup 'b '((a . 1) (b . 2) (c . 3))) ===> 2)

(define (substitute type-scheme #;with substitutions)
  (match type-scheme
    (`(,,type-variable-marker ,_)
     (if (bound? type-scheme #;in substitutions)
	 (lookup type-scheme substitutions)
	 type-scheme))
    (`(forall ,args ,scheme)
     `(forall ,args ,(substitute scheme #;with substitutions)))

    (_
     (if (list? type-scheme)
	 (map (lambda (part)
		(substitute part #;with substitutions))
	      type-scheme)
	 type-scheme))))

(e.g.
 (substitute ',x '((,x . ,y))) ===> ,y)

(e.g.
 (substitute '(Bool -> ,x) '((,x . ,y))) ===> (Bool -> ,y))

(e.g.
 (substitute '(Bool -> ,x) '((,x . Bool))) ===> (Bool -> Bool))

(define (combine substitution . other-substitutions)
  (fold-left (lambda (substitution other)
	       `(,@(map (lambda (`(,k . ,v))
			  `(,k . ,(substitute v substitution)))
			other)
		 ,@substitution))
	     substitution
	     other-substitutions))

(e.g.
 (combine '((,x . ,y)) '((,z . (Bool -> ,x))))
 ===> ((,z . (Bool -> ,y)) (,x . ,y)))

(define unique-symbol-counter
  (make-parameter 0))

(define (unique-symbol base)
  (let ((ordinal (unique-symbol-counter))
	(prefix (cond
		 ((string? base) base)
		 ((symbol? base) (symbol->string base))
		 ((type-variable? base) (symbol->string
				    (type-variable-name base))))))
		      
    (unique-symbol-counter (+ ordinal 1))
    (string->symbol
     (string-append prefix
      "~"(number->string ordinal)))))

(define* (fresh-type-variable #:optional (prefix "T"))
  `(,type-variable-marker ,(unique-symbol prefix)))

(define* (instantiate type-scheme #:optional (mappings '()))
  ;;Type
  (match type-scheme
    (`(forall ,variables ,scheme)
     (let ((mappings* `(,@(map (lambda (variable)
				 `(,variable . ,(fresh-type-variable variable)))
			       variables)
			,@mappings)))
       (instantiate scheme mappings*)))
    
    (`(,,type-variable-marker ,_)
     (if (bound? type-scheme mappings)
	 (lookup type-scheme mappings)
	 type-scheme))
    
    (_
     (if (list? type-scheme)
	 (map (lambda (part)
		(instantiate part mappings))
	      type-scheme)
	 type-scheme))))

(e.g.
 (parameterize ((unique-symbol-counter 0))
   (instantiate
    '(forall (,a ,b) (,a -> ,b))))
 ===> (,a~0 -> ,b~1))

(e.g.
 (parameterize ((unique-symbol-counter 0))
   (instantiate
    '(forall (,y) (forall (,z) (,z -> Bool)))))
 ===> (,z~1 -> Bool))

(define (union a b)
  `(,@(only (isnt _ member a) b) ,@a))

(e.g.
 (union '(a b c) '(b c d)) ===> (d a b c))

(define (free-variables type-scheme)
  (match type-scheme
    (`(forall ,variables ,scheme)
     (difference (free-variables scheme) variables))

    (`(,,type-variable-marker ,_)
     `(,type-scheme))
    
    (`(,_ . ,_)
     (fold-left (lambda (set scheme)
		  (union set (free-variables scheme)))
		'()
		type-scheme))
    
    (_
     '())))

(e.g.
 (free-variables '(forall (,a) (,a -> (,b * String)))) ===> (,b))

(define (difference a b)
  (only (isnt _ member b) a))

(e.g.
 (difference '(a b c) '(b)) ===> (a c))

(define (generalize type type-environment)
  (let ((unbound-variables (difference (free-variables type)
				       (append-map (lambda (`(,k . ,v))
						     (free-variables v))
						   type-environment))))
    (if (null? unbound-variables)
	type
	`(forall ,unbound-variables ,type))))

(e.g.
 (generalize ',t0 '((x . ,t0)))
 ===> ,t0)

(e.g.
 (generalize ',t1 '((x . ,t0)))
 ===> (forall (,t1) ,t1))

(e.g.
 (generalize ',t0 '((x . (forall (,t0) ,t0))))
 ===> (forall (,t0) ,t0))

(e.g.
 (generalize ',t0 '((x . (forall (,t1) ,t0))))
 ===> ,t0)

(define (occurring? variable #;in type-scheme)
  (match type-scheme
    (,variable #t)
    (`(forall ,args ,scheme)
     (and (none (is _ equal? variable) args)
	  (is variable occurring? #;in scheme)))
    (_
     (and (list? type-scheme)
	  (any (is variable occurring? #;in _) type-scheme)))))

(define* (unify x y #:optional (bindings '()))
  (cond
   ((equal? x y) bindings)
   ((type-variable? x)
    (and (isnt x occurring? #;in y)
	 (if (bound? x bindings)
	     (unify (lookup x bindings) y bindings)
	     `((,x . ,y) . ,bindings))))
   ((type-variable? y)
    (unify y x bindings))
   (else
    (and-let* ((`(,x0 . ,x*) x)
	       (`(,y0 . ,y*) y)
	       (bindings* (unify x0 y0 bindings)))
      (unify x* y* bindings*)))))

(e.g.
 (unify '(,a -> ,b) '(Int -> Bool)) ===> ((,b . Bool) (,a . Int)))

(e.g.
 (unify '(,a -> Bool) '(Int -> ,b)) ===> ((,b . Bool) (,a . Int)))

(e.g.
 (not (unify '(,a -> ,a) '(Int -> Bool))))


(define (primitive-type literal)
  (cond
   ((boolean? literal) 'boolean)
   ((number? literal) 'number)
   ((string? literal) 'string)
   ((symbol? literal) 'symbol)
   ((char? literal) 'char)
   ((vector? literal) 'vector)
   ((port? literal) 'port)
   ((null? literal) '(forall (,a) (list-of ,a)))
   (else (error "Primitive object of unknown type: "literal))))

(define initial-type-environment
  '((cons  . (forall (,a) (maps (,a (list-of ,a)) to: (list-of ,a))))
    (car   . (forall (,a) (maps (list-of ,a) to: ,a)))
    (cdr   . (forall (,a) (maps (list-of ,a) to: (list-of ,a))))
    (null? . (forall (,a) (maps (list-of ,a) to: boolean)))
    
    (not   . (boolean -> boolean))
    (odd?  . (number -> boolean))
    (even? . (number -> boolean))

    ))


;; "Algorithm W"
(define* (type+substitutions expression
			     #:optional
			     (type-environment initial-type-environment))
  (match expression
    (`(let ((,variable ,value)) ,body)
     (let* ((value-type substitutions
			(type+substitutions
			 value
			 type-environment))
	    (type-environment* (map (lambda (`(,identifier . ,type))
				      `(,identifier . ,(substitute type substitutions)))
				    type-environment))
	    (body-type substitutions*
		       (type+substitutions
			body
			`((,variable . ,(generalize value-type type-environment*))
			  . ,type-environment*))))
       (values body-type (combine substitutions substitutions*))))

    (`(lambda (,arg) ,body)
     (let* ((arg-type (fresh-type-variable))
	    (body-type substitutions
		       (type+substitutions 
			body
			`((,arg . ,arg-type) . ,type-environment))))
       (values (substitute `(,arg-type -> ,body-type) substitutions)
	       substitutions)))
    
    (`(,function ,object)
     (let* ((function-type substitutions
			   (type+substitutions
			    function
			    type-environment))
	    (object-type substitutions*
			 (type+substitutions
			  object
			  (map (lambda (`(,identifier . ,type))
				 `(,identifier . ,(substitute type substitutions)))
			       type-environment)))
	    (result-type (fresh-type-variable))
	    (substitutions** (unify (substitute function-type
						substitutions*)
				    `(,object-type -> ,result-type))))
       (values (substitute result-type substitutions**)
	       (combine substitutions substitutions* substitutions**))))
    (_
     (values
      (if (symbol? expression)
	  (let ((expression-type (lookup expression type-environment)))
	    (instantiate expression-type))
	  (primitive-type expression))
      '()))))

(e.g.
 (parameterize ((unique-symbol-counter 0))
   (type+substitutions '(lambda (x) x)))
 ===> (,T~0 -> ,T~0) ())


(e.g.
 (parameterize ((unique-symbol-counter 0))
   (type+substitutions '(not #t)))
 ===> boolean ((,T~0 . boolean)))

(e.g.
 (parameterize ((unique-symbol-counter 0))
   (type+substitutions '(odd? 1)))
 ===> boolean ((,T~0 . boolean)))

(e.g.
 (parameterize ((unique-symbol-counter 0))
   (type+substitutions '(let ((o odd?))
			  (o ((lambda (x) x) 1)))))
 ===> boolean ((,T~2 . boolean) (,T~1 . number) (,T~0 . number)))


(define* (type-of expression #:optional (type-environment initial-type-environment))
  (let* ((type substitutions (type+substitutions expression type-environment)))
    (generalize type type-environment)))


(e.g.
 (parameterize ((unique-symbol-counter 0))
   (type-of '(lambda (x) x))) ===> (forall (,T~0) (,T~0 -> ,T~0)))


;; The M algorithm
(define (substitutions-unifying expression #;with type #;in type-environment)
  (match expression
    (`(lambda (,arg) ,body)
     (let* ((arg-type (fresh-type-variable))
	    (body-type (fresh-type-variable))
	    (function-type-unifier (unify type `(,arg-type -> ,body-type)))
	    (type-environment* (map (lambda (`(,identifier . ,type))
				      `(,identifier . ,(substitute type function-type-unifier)))
				    type-environment))
	    (arg-type* (substitute arg-type function-type-unifier))
	    (body-type* (substitute body-type function-type-unifier))
	    (body-type-unifier (substitutions-unifying
				body #;with body-type*
				#;in `((,arg . ,arg-type*) . ,type-environment*))))
       (combine function-type-unifier body-type-unifier)))

    (`(let ((,variable ,value)) ,body)
     (let* ((value-type (fresh-type-variable))
	    (value-type-unifier (substitutions-unifying value #;with value-type
							#;in type-environment))
	    (type* (substitute type value-type-unifier))
	    (type-environment* (map (lambda (`(,identifier . ,type))
				      `(,identifier . ,(substitute type value-type-unifier)))
				    type-environment))
	    (variable-type (generalize (substitute value-type value-type-unifier)
				       type-environment*))
	    (body-type-unifier (substitutions-unifying body #;with type*
						       #;in type-environment*)))
       (combine value-type-unifier body-type-unifier)))

    (`(,function ,argument)
     (let* ((argument-type (fresh-type-variable))
	    (function-type-unifier (substitutions-unifying function #;with
							   `(,argument-type -> ,type)
							   #;in type-environment))
	    (argument-type* (substitute argument-type function-type-unifier))
	    (type-environment* (map (lambda (`(,identifier . ,type))
				      `(,identifier . ,(substitute type function-type-unifier)))
				    type-environment))
	    (argument-type-unifier (substitutions-unifying argument #;with argument-type*
							   #;in type-environment*)))
       (combine function-type-unifier argument-type-unifier)))
    (_
     (unify type
	    (if (symbol? expression)
		(let ((expression-type (lookup expression type-environment)))
		  (instantiate expression-type))
		(primitive-type expression))))))


(define* (type-of* expression #:optional (type-environment initial-type-environment))
  (let* ((type (fresh-type-variable))
	 (substitution (substitutions-unifying expression #;with type #;in type-environment)))
    (substitute type substitution)))

(parameterize ((unique-symbol-counter 0))
   (type-of* '(lambda (x) x)))
