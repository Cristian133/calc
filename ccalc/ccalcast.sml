datatype Expr = Nro of int
	| Var of string
	| Suma of Expr * Expr
	| Resta of Expr * Expr
	| Prod of Expr * Expr
	| Div of Expr * Expr
	| Cmp of oper * Expr * Expr
	| Opuesto of Expr
	| Print of Expr
	| Asign of string * Expr
	| If of Expr * Expr * Expr
	| While of Expr * Expr
	| Sec of Expr * Expr
	| Nop
and oper = Menor | MenIg | Mayor | MayIg | Igual | Dist
