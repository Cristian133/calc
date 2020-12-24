open tfuncast

local
	val n = ref 0
in
	fun newId() = "_f"^Int.toString(!n) before n := !n + 1
end
fun emitNro n = (if n<0 then "-" else "")^Int.toString(Int.abs n)
fun emitTexto s = "s\" "^s^"\" "
fun emitId s = "_"^s^" "
fun emitAri(oper, e1, e2) =
			e1^" "^e2^
			(case oper of
			Suma => " +"
			| Resta => " -"
			| Prod => " *"
			| Div => " /")^"\n"
fun emitCall f params = List.foldr (fn(x, y) => x^" "^y) (" "^f^"\n") params
fun emitSeq e1 e2 = e1^"\n"^e2^"\n"
fun emitAsign s e = e^" to _"^s^"\n"
fun emitIf e1 e2 e3 = e1^"\nif\n"^e2^"\nelse\n"^e3^"\nendif\n"
fun emitCmp comp e1 e2 =
	e1^" "^e2^" "^
	(case comp of
	Mayor => ">"
	| Menor => "<"
	| MayIg => ">="
	| MenIg => "<="
	| Igual => "="
	| Dist => "<>\n")^"\n"
fun emitPrint e t =
	(case t of
	String => e^" type "
	| Int => e^" . "
	| _ => raise Fail "error interno")^"\n"
fun emitCr() = " cr\n"
fun emitRead e t =
	(case t of
	Int => "getint to "^e^"\n"
	| String => "accept to "^e^"\n"
	| _ => raise Fail "error interno")
fun emitLet e = " "^e^"\n"
fun emitFunc f (args:(string*Tipo) list) body =
	let	fun emitargs[] = ""
		| emitargs(h::t) = "_"^h^" "^emitargs t
	in
		":noname "^"{ "^emitargs (List.map (#1) args)^"}\n"^
		body^";\nis "^f^"\n"
	end
