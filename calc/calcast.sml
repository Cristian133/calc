datatype Expr = Nro of int
	| Suma of Expr * Expr
	| Resta of Expr * Expr
	| Prod of Expr * Expr
	| Div of Expr * Expr
	| Opuesto of Expr
	| Print of Expr
	| Nop

fun emit(Nro n) =
	if n<0 then "-"^Int.toString(~n) else Int.toString n
| emit(Suma(e1, e2)) = emit e1^" "^emit e2^" +\n"
| emit(Resta(e1, e2)) = emit e1^" "^emit e2^" -\n"
| emit(Prod(e1, e2)) = emit e1^" "^emit e2^" *\n"
| emit(Div(e1, e2)) = emit e1^" "^emit e2^" /\n"
| emit(Opuesto e) = emit e^" negate\n"
| emit(Print e) = emit e^" . cr bye"
| emit Nop = ""
