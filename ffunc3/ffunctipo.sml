datatype Tipo = TUnit
	| TInt
	| TString
	| TT of Tipo list
	| TF of Tipo * Tipo

fun toStr TUnit = "unidad"
| toStr TInt = "entero"
| toStr TString = "cadena"
| toStr(TT lt) =
	let	fun st [] = ""
		| st [t] = toStr t
		| st (h::t) = toStr h^","^st t
	in	"("^st lt^")" end
| toStr (TF(t1, t2)) = "(("^toStr t1^")->"^toStr t2^")"
