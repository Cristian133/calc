datatype Comp=Mayor|Menor|MayIg|Igual|Dist
datatype Expr= Nro of int
		| Id of string
		| Suma of Expre * Expr
		| Resta of Expr * Expr
		| Prod of Expre * Expr
		| Div of Expr * Expr
		| Call of string * Expr list
		| See of Expr * Expr
		| Asign of string * Expr
		| If of Expr * Expr * Expr
		| Cmp of Comp * Expr * Expr
		| Print of Expr
		| Cr
		| Read of Expr
datatype Decl = Func of {func:string, args:string list, body:Expr}

(* para mostrar el AST *)
fun ppExpr(Nro n)="Nro("^Int.toString n^")"
|   ppExpr(Id s)="ID("^s^)")"
|   ppExpr(Suma e1, e2)="Suma("^ppexpr e1^","ppExpr e2^")"
	.
	.
	.
	.
	etc

fun ppDecl(Func{func, args, body})=
	let fun inter[]=""
		|   inter[a]=a
		|   inter(h::t)=h^", "^inter t
	in func^"("^inter args^")="^ppExpr body ^"\n" end
		

fun  ppDecl[]=""
|   ppDecl(h::t)=ppDecl h^ ppDecl t


