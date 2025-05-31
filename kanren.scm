(use-modules (grand scheme))

(define (lookup variable bindings)
  (match bindings
    (`((,,variable . ,value) . ,_) value)
    (`((,_ . ,_) . ,sequel) (lookup variable sequel))))

(e.g.
 (lookup 'b '((a . 1) (b . 2) (c . 3))) ===> 2)

(define (variable? x)
  (and-let* ((`(,'unquote ,value) x))))

(e.g.
 (lookup ',b '((,a . 1) (,b . 2) (,c . 3))) ===> 2)

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
     (if (variable? expression)
	 (lookup expression bindings)
	 expression))))

(define (unify x y bindings)
  (cond
   ((not bindings) #f)
   ((equal? x y) bindings)
   ((variable? x)
    (if (bound? x bindings)
	(unify (lookup x bindings) y bindings)
	`((,x . ,y) . ,bindings)))
   ((variable? y)
    (unify y x bindings))
   (else
    (and-let* ((`(,x0 . ,x*) x)
	       (`(,y0 . ,y*) y))
      (unify x* y* (unify x0 y0 bindings))))))

(unify '(stole John ,X)
       '(stole ,Y the-car) '())

