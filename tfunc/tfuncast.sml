open tfunctipo

datatype Arit = Suma | Resta | Prod | Div
datatype Comp = Mayor | Menor | MayIg | MenIg | Igual | Dist
datatype Decl =
	Func of { f: string, args: (string*Tipo) list, res: Tipo, body: Expr }
	| Var of string*Tipo
and Expr = Nro of int
	| Texto of string
	| Id of string
	| Ari of Arit * Expr * Expr
	| Call of string * Expr list
	| Sec of Expr * Expr
	| Asign of string * Expr
	| If of Expr * Expr * Expr
	| Cmp of Comp * Expr * Expr
	| Print of Expr
	| Cr
	| Read of Expr
	| Let of Decl list * Expr

fun ppExpr(Nro n) = "(Nro "^Int.toString n^")"
| ppExpr(Texto s) = "(Texto "^s^")"
| ppExpr(Id s) = "(Id "^s^")"
| ppExpr(Ari(oper, e1, e2)) =
	(case oper of
	Suma => "(Suma("
	| Resta => "(Resta("
	| Prod => "(Prod("
	| Div => "(Div(")^ppExpr e1^","^ppExpr e2^")"
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
| ppExpr(Let(decs, exp)) =
	"Let("^ppDecls decs^","^ppExpr exp^")"
| ppExpr Cr = "(Cr)"
and ppDecl(Func{f, args, res, body}) =
	let	fun inter [] = ""
		| inter [(s, ts)] = s^":"^toStr ts
		| inter((h, th)::t) = h^":"^toStr th^","^inter t
	in	f^"("^inter args^"):"^toStr res^"="^ppExpr body end
| ppDecl(Var(s, t)) = "Var("^s^":"^toStr t^")"
and ppDecls [] = ""
| ppDecls(h::t) = ppDecl h^"\n"^ppDecls t
