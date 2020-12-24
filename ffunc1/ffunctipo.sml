datatype Tipo = TUnit
	| TGen
	| TInt
	| TString
	| TT of Tipo list
	| TF of Tipo * Tipo

fun toStr TUnit = "unit"
| toStr TGen = "gen"
| toStr TInt = "int"
| toStr TString = "string"
| toStr(TT lt) =
	"("^toStr(hd lt)^(if length lt>1 then "," else "")^
	List.foldr(fn(t, s) => toStr t^","^s) ")" (rev(tl(rev lt)))
| toStr (TF(t1, t2)) = "("^toStr t1^"->"^toStr t2^")"
