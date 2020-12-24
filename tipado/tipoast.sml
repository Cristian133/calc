datatype Tipo = Int
	| String
	| Unit
	| Tupla of Tipo list
	| Flecha of Tipo * Tipo

fun ppTipo Int = "int"
| ppTipo String = "string"
| ppTipo Unit = "unit"
| ppTipo(Tupla lt) =
	let	fun aux [] = ""
		| aux [t] = ppTipo t
		| aux(h::t) = ppTipo h^","^aux t
	in	"("^aux lt^")" end
| ppTipo(Flecha(t1, t2)) = "("^ppTipo t1^"->"^ppTipo t2^")"
