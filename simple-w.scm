(use-modules (grand scheme))


;; No dobra, to moze zamiast probowac zgadywac
;; tajemnicze symbole, sprobujmy odtworzyc
;; samo rozumowanie?


(define (map. f l)
  (match l
    ('()        '())
    (`(,h . ,t) `(,(f h) . ,(map. f t)))
    (_          (f l))))

(e.g.
 (map. (lambda (x) (* x x))
       '(1 2 . 3)) ===> (1 4 . 9))


(define (lookup variable bindings)
  (match bindings
    (`((,,variable . ,value) . ,_) value)
    (`((,_ . ,_) . ,sequel) (lookup variable sequel))))

(e.g.
 (lookup ',b '((,a . 1) (,b . 2) (,c . 3))) ===> 2)

(define (var? x)
  (and-let* ((`(,'unquote ,value) x))))

(define unique-symbol-counter
  (make-parameter 0))

(define (unique-symbol base-symbol)
  (let ((ordinal (unique-symbol-counter)))
    (unique-symbol-counter (+ ordinal 1))
    (string->symbol
     (string-append
      (symbol->string base-symbol)
      "~"(number->string ordinal)))))

(define (fresh-var)
  (list 'unquote (unique-symbol 'T)))

(define (empty-substitutions)
  '())


(define (bound? variable #;in bindings)
  (match bindings
    (`((,,variable . ,_) . ,_) #t)
    (`((,_ . ,_) . ,sequel) (bound? variable #;in sequel))
    ('() #f)))

(e.g.
 (bound? ',b #;in '((,a . 1) (,b . 2) (,c . 3))))

(e.g.
 (isnt ',d bound? #;in '((,a . 1) (,b . 2) (,c . 3))))

(define (substitute expression bindings)
  (match expression
    (`(quote ,_)
     expression)
    (`(,head . ,tail)
     `(,(substitute head bindings) . ,(substitute tail bindings)))
    (_
     (if (var? expression)
	 (lookup expression bindings)
	 expression))))

(define (unify x y bindings)
  (cond
   ((not bindings) #f)
   ((equal? x y) bindings)
   ((var? x)
    (if (bound? x bindings)
	(unify (lookup x bindings) y bindings)
	`((,x . ,y) . ,bindings)))
   ((var? y)
    (unify y x bindings))
   (else
    (and-let* ((`(,x0 . ,x*) x)
	       (`(,y0 . ,y*) y))
      (unify x* y* (unify x0 y0 bindings))))))

(e.g.
 (unify '(stole John ,X)
	'(stole ,Y the-car) '()) ===> ((,X . the-car) (,Y . John)))


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
  '((cons  . (forall (a) (maps (a (list-of a)) to: (list-of a))))
    (car   . (forall (a) (maps (list-of a) to: a)))
    (cdr   . (forall (a) (maps (list-of a) to: (list-of a))))
    (null? . (forall (a) (maps (list-of a) to: boolean)))))

(type+substitution '(define map
		      (lambda (f l)
			(if (null? l)
			    '()
			    (cons (f (car l)) (map f (cdr l))))))
		   initial-type-environment)


(define (primitive-type literal)
  (cond
   ((boolean? literal) 'boolean)
   ((number? literal) 'number)
   ((string? literal) 'string)
   ((symbol? literal) 'symbol)
   ((char? literal) 'char)
   ((vector? literal) 'vector)
   ((port? literal) 'port)
   (else (error "Primitive object of unknown type: "literal))))
    
(define (extend type-environment #;with substitutions)
  `(,@substitutions ,@type-environment))

(define (augment type-environment #;with . new-bindings)
  (fold-left (lambda (type-environment bindings)
	       `(,bindings . ,type-environment))
	     type-environment
	     new-bindings))

(define (merge substitutions #;with . other-substitutions)
  ...)

(define (lookup type-variable #;in type-environment)
  ...)

(define (type-instance expression-type)
  ...)

(define (type+substitutions expression type-environment)
  (match expression
    (`(let ((,variable ,value)) ,body)
     (let* ((value-type substitutions
			(type+substitutions
			 value
			 type-environment))
	    (body-type substitutions*
		       (type+substitutions
			body
			(augment
			 (extend type-environment
				 #;with substitutions)
			 ;; 
			 `(,variable . ,???)))))
       (values body-type (merge substitutions substitutions*))))

    (`(lambda (,arg) ,body)
     (let* ((arg-type (new-type-variable ))
	    (body-type substitutions
		       (type+substitutions 
			body
			(augment
			 type-environment
			 `(,arg . ,arg-type)))))
       (values `(,(lookup arg-type type-environment) -> ,body-type)
	       substitutions)))
        
    (`(,function ,object)
     (let* ((function-type substitutions
			   (type+substitutions
			    function
			    type-environment))
	    (object-type substitutions*
			 (type+substitutions
			  object
			  (extend type-environment
				  #;with substitutions)))
	    (result-type (new-type-variable))
	    (substitutions** (unify (lookup function-type
					    substitutions*)
				    `(,object-type -> ,result-type))))
       (values (lookup result-type substitutions**)
	       (merge substitutions**
		      substitutions*
		      substitutions))))
    (_
     (values
      (if (symbol? expression)
	  (let ((expression-type (lookup expression type-environment)))
	    (type-instance expression-type))
	  (primitive-type expression))
      (empty-substitutions)))))
