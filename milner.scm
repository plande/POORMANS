(use-modules (poormans guile))

(read-set! keywords 'postfix)

(define-type Expression
  (Variable name: String)
  (Application of: Expression
	       to: Expression)
  (Lambda of: Variable
	  is: Expression)
  (Let variable: Variable
       be: Expression
       in: Expression))

(define-type Type
  (TypeVariable name: String)
  (Boolean of: (Either #f #t))
  (Integer of: integer)
  (List of: Type)
  (Maps from: Type to: Type))

(define-type PolyType
  (Grounded type: Type)
  (TypeQuantifier of: TypeVariable
		  in: PolyType))
