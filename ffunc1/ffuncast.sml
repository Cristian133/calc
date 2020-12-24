open ffunctipo

datatype Arit = Suma | Resta | Prod | Div
datatype Comp = Mayor | Menor | MayIg | MenIg | Igual | Dist
datatype Expr = Nro of int
	| Texto of string
	| Unit
	| Id of string
	| Tupla of Expr list
	| Oper of Arit * Expr * Expr
	| App of Expr * Expr
	| Call of string * Expr
	| Sec of Expr * Expr
	| Asign of string * Expr
	| If of Expr * Expr * Expr
	| Cmp of Comp * Expr * Expr
	| Print of Expr
	| Cr
	| Read of string
	| Lambda of string * Tipo * Expr * Tipo
	| Let of Decl list * Expr
	| Nop
and Decl = Func of {func: string, arg: string * Tipo, body: Expr, tipo: Tipo}
	| Var of string*Tipo

fun ppExpr(Nro n) = "(Nro "^Int.toString n^")"
| ppExpr(Texto s) = "(Texto "^s^")"
| ppExpr(Id s) = "(Id "^s^")"
| ppExpr Unit = "(Unit)"
| ppExpr(Tupla lt) =
	let	fun inter [] = ""
		| inter [s] = ppExpr s
		| inter(h::t) = ppExpr h^","^inter t
	in	"("^inter lt^")" end
| ppExpr Nop = "(Nop)"
| ppExpr(Oper(oper, e1, e2)) =
	(case oper of
	Suma => "(Suma("
	| Resta => "(Resta("
	| Prod => "(Prod("
	| Div => "(Div(")^ppExpr e1^","^ppExpr e2^")"
| ppExpr(App(e1, e2)) = "(App("^ppExpr e1^","^ppExpr e2^")"
| ppExpr(Call(s, arg)) =
	"(Call("^s^ppExpr arg^")"
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
| ppExpr Cr = "(Cr)"
| ppExpr(Read s) = "(Read "^s^")"
| ppExpr(Let(decs, exp)) = "(Let "^ppDecls decs^","^ppExpr exp^")"
| ppExpr(Lambda(s, ts, e, te)) = "(Lambda("^s^":"^toStr ts^","^ppExpr e^"):"^toStr te^")"
and ppDecl(Func{func, arg, body, tipo}) =
	func^"("^(#1 arg)^":"^toStr(#2 arg)^")="^ppExpr body^":"^toStr tipo
| ppDecl(Var(s, t)) = "*Var "^s^":"^toStr t^")"
and ppDecls [] = ""
| ppDecls(h::t) = ppDecl h^"\n"^ppDecls t
