

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

(define-type MonoType
  (TypeVariable name: String)
  (TypeApplication of: TypeFunction
		   to: (list-of MonoType)))

(define-type PolyType
  ()
  (TypeQuantifier of: TypeVariable
		  in: PolyType))
