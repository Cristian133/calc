open ffunctipo

fun gmktp [] _ = "\n"
| gmktp(TString::t) cnt = "-rot _mkstring swap 2! "^gmktp t (cnt+1)
| gmktp((TT lt)::t) cnt = (gmktp lt 0)^gmktp t (cnt+1)
| gmktp(h::t) cnt = " swap !"^gmktp t (cnt+1)
fun genmktp lt =
	"here "^Int.toString(length lt)^" cells allot\n"^
	gmktp lt 0 

val lt = [TString, TInt, TString]
(*
datatype Tipo = TUnit
	| TInt
	| TString
	| TT of Tipo list
	| TF of Tipo * Tipo

fun toStr TUnit = "unit"
| toStr TInt = "int"
| toStr TString = "string"
| toStr(TT lt) =
	let	fun st [] = ""
		| st [t] = toStr t
		| st (h::t) = toStr h^","^st t
	in	"("^st lt^")" end
| toStr (TF(t1, t2)) = "(("^toStr t1^")->"^toStr t2^")"
*)
