open funcast

local
	val n = ref 0
in
	fun newId() = "_f"^Int.toString(!n) before n := !n + 1
end
fun emitEx(Nro n) =
	(if n<0 then "-" else "")^Int.toString(Int.abs n)
| emitEx(Id s) = "_"^s^" "
| emitEx(Suma(e1, e2)) = emitEx e1^" "^emitEx e2^" +\n"
| emitEx(Resta(e1, e2)) = emitEx e1^" "^emitEx e2^" -\n"
| emitEx(Prod(e1, e2)) = emitEx e1^" "^emitEx e2^" *\n"
| emitEx(Div(e1, e2)) = emitEx e1^" "^emitEx e2^" /\n"
| emitEx(Call(s, elst)) =
	(case Polyhash.peek funcenv.env s of
	SOME n =>
		if length elst<>n then raise Fail (s^" con args inconvenientes\n")
		else ()
	| _ => raise Fail (s^" inexistente\n");
	(List.foldr (fn(x, y) => emitEx x^" "^y) "" elst)^s^"\n")
| emitEx(Sec(e1, e2)) = emitEx e1^"\n"^emitEx e2
| emitEx(Asign(s, e)) = emitEx e^" to _"^s^"\n"
| emitEx(If(e1, e2, e3)) =
	emitEx e1^"\nif\n"^emitEx e2^"\nelse\n"^emitEx e3^"\nendif\n"
| emitEx(Cmp(Comp, e1, e2)) =
	emitEx e1^" "^emitEx e2^" "^
	(case Comp of
	Mayor => ">"
	| Menor => "<"
	| MayIg => ">="
	| MenIg => "<="
	| Igual => "="
	| Dist => "<>\n")^"\n"
| emitEx(Print e) = emitEx e^" . \n"
| emitEx Cr = " cr\n"
| emitEx(Read e) =
	(case e of
	Id v => "getint to _"^v^"\n"
	| _ => raise Fail "read a algo que no es variable!\n")

fun emitDecl(Func{func, args, body}) =
	let	fun emitargs[] = ""
		| emitargs(h::t) = "_"^h^" "^emitargs t
	in
		": "^func^" recursive\n"^"{ "^emitargs args^"}\n"
		^emitEx body^";\n"
	end
fun emitDecls [] = ""
| emitDecls(h::t) = emitDecl h^emitDecls t
