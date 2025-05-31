(use-modules (grand scheme))

(define unique-symbol-counter
  (make-parameter 0))

(define (unique-symbol base-symbol)
  (let ((ordinal (unique-symbol-counter)))
    (unique-symbol-counter (+ ordinal 1))
    (string->symbol
     (string-append
      (symbol->string base-symbol)
      "~"(number->string ordinal)))))

(define (fresh-type-variable)
  (unique-symbol 'T))

(define (empty-substitutions)
  '())

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
  ...)

(define (augment type-environment #;with . new-bindings)
  ...)

(define (merge substitutions #;with . other-substitutions)
  ...)

(define (lookup type-variable #;in type-environment)
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
