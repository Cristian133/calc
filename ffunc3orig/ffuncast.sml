open ffunctipo

datatype Arit = Suma | Resta | Prod | Div
datatype Comp = Mayor | Menor | MayIg | MenIg | Igual | Dist
datatype Expr = Nro of int
	| Texto of string
	| Unit
	| Id of string
	| Tupla of Expr list
	| Memb of Expr * int
	| Oper of Arit * Expr * Expr
	| Call of Expr * Expr
	| Sec of Expr * Expr
	| Asign of Expr * Expr
	| If of Expr * Expr * Expr
	| Cmp of Comp * Expr * Expr
	| Print of Expr
	| Cr
	| Read of string
	| Let of Decl list * Expr
	| Nop
and Decl = Func of {f: string, arg: string * Tipo, body: Expr, tipo: Tipo, pr:int}
	| Var of {v:string, t:Tipo, pr: int}

fun ppExpr(Nro n) = "(Nro "^Int.toString n^")"
| ppExpr(Texto s) = "(Texto \""^s^"\")"
| ppExpr(Id s) = "(Id "^s^")"
| ppExpr Unit = "(Unit)"
| ppExpr(Tupla lt) =
	let	fun inter [] = ""
		| inter [s] = ppExpr s
		| inter(h::t) = ppExpr h^","^inter t
	in	"Tupla("^inter lt^")" end
| ppExpr(Memb(e, n)) = "Memb "^ppExpr e^"."^Int.toString n^")"
| ppExpr Nop = "(Nop)"
| ppExpr(Oper(oper, e1, e2)) =
	(case oper of
	Suma => "(Suma("
	| Resta => "(Resta("
	| Prod => "(Prod("
	| Div => "(Div(")^ppExpr e1^","^ppExpr e2^")"
| ppExpr(Call(s, arg)) =
	"(Call("^ppExpr s^","^ppExpr arg^")"
| ppExpr(Sec(e1, e2)) = ppExpr e1^";"^ppExpr e2
| ppExpr(Asign(s, e)) = "(Asign("^ppExpr s^","^ppExpr e^"))"
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
| ppExpr(Let(decs, exp)) = "(Let "^ppDecls decs^",body=("^ppExpr exp^")"
and ppDecl(Func{f, arg, body, tipo, pr}) =
		f^"("^(#1 arg)^":"^toStr(#2 arg)^")="^ppExpr body^":"^toStr tipo^","^Int.toString pr
| ppDecl(Var{v, t, pr}) = "*Var "^v^":"^toStr t^","^Int.toString pr^")"
and ppDecls [] = ""
| ppDecls(h::t) = ppDecl h^"\n"^ppDecls t
