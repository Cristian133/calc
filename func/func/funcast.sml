datatype Comp = Mayor | Menor | MayIg | MenIg | Igual | Dist
datatype Expr = Nro of int
	| Id of string
	| Suma of Expr * Expr
	| Resta of Expr * Expr
	| Prod of Expr * Expr
	| Div of Expr * Expr
	| Call of string * Expr list
	| Sec of Expr * Expr
	| Asign of string * Expr
	| If of Expr * Expr * Expr
	| Cmp of Comp * Expr * Expr
	| Print of Expr
	| Cr
	| Read of Expr
datatype Decl = Func of { func: string, args: string list, body: Expr }

fun ppExpr(Nro n) = "(Nro "^Int.toString n^")"
| ppExpr(Id s) = "(Id "^s^")"
| ppExpr(Suma(e1, e2)) = "(Suma("^ppExpr e1^","^ppExpr e2^")"
| ppExpr(Resta(e1, e2)) = "(Resta("^ppExpr e1^","^ppExpr e2^")"
| ppExpr(Prod(e1, e2)) = "(Prod("^ppExpr e1^","^ppExpr e2^")"
| ppExpr(Div(e1, e2)) = "(Div("^ppExpr e1^","^ppExpr e2^")"
| ppExpr(Call(s, args)) =
	let	fun inter [] = ""
		| inter [s] = ppExpr s
		| inter(h::t) = ppExpr h^","^inter t
	in	"(Call("^inter args^"))" end
| ppExpr(Sec(e1, e2)) = ppExpr e1^";"^ppExpr e2
| ppExpr(Asign(s, e)) = "(Asign("^s^","^ppExpr e^"))"
| ppExpr(If(e1, e2, e3)) = "(If("^ppExpr e1^","^ppExpr e2^","^ppExpr e3^"))"
| ppExpr(Cmp(c, e1, e2)) =
	let	val comp =
			case c of
			Mayor => "<"
			| Menor => "<"
			| MayIg => ">="
			| MenIg => "<="
			| Igual => "="
			| Dist => "<>"
	in	"(Cmp("^comp^","^ppExpr e1^","^ppExpr e2^"))" end
| ppExpr(Print e) = "(Print "^ppExpr e^")"
| ppExpr(Read e) = "(Read "^ppExpr e^")"
| ppExpr Cr = "(Cr)"
fun ppDecl(Func{func, args, body}) =
	let	fun inter [] = ""
		| inter [s] = s
		| inter(h::t) = h^","^inter t
	in	func^"("^inter args^")="^ppExpr body end
fun ppDecls [] = ""
| ppDecls(h::t) = ppDecl h^"\n"^ppDecls t
