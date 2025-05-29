(define-module (poormans guile)
  #:use-module (grand scheme)
  #:use-module (oop goops)
  #:export-syntax (define-type))

(define (symbol+keyword+type+value keyword+attribute-list)
  (let loop ((input keyword+attribute-list))
    (match input
      (`(,keyword ,type #:= ,init-value . ,rest)
       `((,(keyword->symbol keyword)
	  ,keyword
	  ,type
	  ,init-value) . ,(loop rest)))
      
      (`(,keyword ,type . ,rest)
       `((,(keyword->symbol keyword)
	  ,keyword
	  ,type
	  #f) . ,(loop rest)))
      (_ '()))))

(define ((with-suffix suffix) base-class-name)
  (string->symbol
   (string-append (symbol->string base-class-name)
		  (symbol->string suffix))))

(define-syntax define-variant
  (lambda (stx)
    (define (apply-to-datum f arg)
      (datum->syntax stx (f (syntax->datum arg))))

    (syntax-case stx ()
      ((define-variant superclass (variant-name spec ...))
       (with-syntax ((variant-name% (apply-to-datum (with-suffix '%) #'variant-name))
		     (((symbol keyword type value) ...)
		      (apply-to-datum symbol+keyword+type+value #'(spec ...))))
	 #'(begin
	     (define-class variant-name% superclass
	       (symbol #:init-keyword keyword #:init-value value)
	       ...)
	     (define-method (write (object variant-name%) (port <output-port>))
	       (write-char #\[ port)
	       (write 'variant-name port)
	       (begin
		 (write-char #\space port)
		 (write 'symbol port)
		 (write-char #\: port)
		 (write-char #\space port)
		 (write (slot-ref object 'symbol) port))
	       ...
	       (write-char #\] port))

	     (define-method (equal? (object variant-name%) another . rest)
	       (and (is-a? another variant-name%)
		    (equal? (slot-ref object 'symbol) (slot-ref another 'symbol))
		    ...
		    (or (null? rest)
			(apply equal? object rest))))

	     ;; jeszcze powinnismy zbudowac nieco metadanych dla obiektu/klasy
	     (define (variant-name . args)
	       (apply make variant-name% args))))))))

(define-syntax define-type
  (syntax-rules ()
    ((define-type (TypeName spec ...))
     (define-variant () (TypeName spec ...)))

    ((define-type TypeName
       (Variant spec ...)
       ...)
     (begin
       (define-class TypeName ())
       (define-variant (TypeName) (Variant spec ...))
       ...))))


