;; Code based on Adam Jones' implementation of Hindley-Milner
;; type inference algorithm that can be found at
;; https://github.com/domdomegg/hindley-milner-typescript.git
;; with video tutorial available at
;; https://www.youtube.com/@adam-jones/videos

(use-modules (grand scheme))

(define variable-marker 'unquote)

(define (variable? x)
  (and-let* ((`(,,variable-marker ,value) x))))

(e.g.
 (variable? ',x))

(e.g.
 (isnt 'x variable?))

(define (variable-name variable)
  (match variable
    (`(,,variable-marker ,name)
     name)))

(e.g.
 (variable-name ',x) ===> x)

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

(define unique-symbol-counter
  (make-parameter 0))

(define (unique-symbol base)
  (let ((ordinal (unique-symbol-counter))
	(prefix (cond
		 ((string? base) base)
		 ((symbol? base) (symbol->string base))
		 ((variable? base) (symbol->string
				    (variable-name base))))))
		      
    (unique-symbol-counter (+ ordinal 1))
    (string->symbol
     (string-append prefix
      "~"(number->string ordinal)))))

(define* (fresh-variable #:optional (prefix "T"))
  `(,variable-marker ,(unique-symbol prefix)))

(define* (instantiate type-scheme #;TypeScheme #:optional (mappings '()) #;(maps from: var to: Type))
  ;;Type
  (match type-scheme
    (`(forall ,variables ,scheme)
     (let ((mappings* `(,@(map (lambda (variable)
				 `(,variable . ,(fresh-variable variable)))
			       variables)
			,@mappings)))
       (instantiate scheme mappings*)))
    
    (`(,,variable-marker ,variable)
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

(define (free-variables type-scheme)
  (match type-scheme
    (`(forall ,variables ,scheme)
     (difference (free-variables scheme) variables))

    (`(,,variable-marker ,variable)
     `(,type-scheme))
    
    (`(,_ . ,_)
     (append-map free-variables type-scheme))
    
    (_
     '())))

(e.g.
 (free-variables '(forall (,a) (,a -> (,b * String)))) ===> (,b))

(define (difference a b)
  (only (isnt _ member b) a))

(e.g.
 (difference '(a b c) '(b)) ===> (a c))

(define (generalize type #;Type type-environment)
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
   ((not bindings) #f)
   ((equal? x y) bindings)
   ((variable? x)
    (and (isnt x occurring? #;in y)
	 (if (bound? x bindings)
	     (unify (lookup x bindings) y bindings)
	     `((,x . ,y) . ,bindings))))
   ((variable? y)
    (unify y x bindings))
   (else
    (and-let* ((`(,x0 . ,x*) x)
	       (`(,y0 . ,y*) y))
      (unify x* y* (unify x0 y0 bindings))))))

(e.g.
 (unify '(,a -> ,b) '(Int -> Bool)) ===> ((,b . Bool) (,a . Int)))

(e.g.
 (unify '(,a -> Bool) '(Int -> ,b)) ===> ((,b . Bool) (,a . Int)))

(e.g.
 (not (unify '(,a -> ,a) '(Int -> Bool))))


