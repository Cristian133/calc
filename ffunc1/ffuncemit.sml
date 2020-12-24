open ffuncast

local
	val n = ref 0
in
	fun newId() = "_f"^Int.toString(!n) before n := !n + 1
end
fun emitNro n = (if n<0 then "-" else "")^Int.toString(Int.abs n)
fun emitTexto s = "s\" "^s^"\" "
fun emitUnit() = "0 "
fun emitId s b =
	(if b then " ' " else "")^
	(if hd(explode s)= #"_" then s else "_"^s)^" "
fun emitTupla le = ""
fun emitAri(oper, e1, e2) =
			e1^" "^e2^
			(case oper of
			Suma => " +"
			| Resta => " -"
			| Prod => " *"
			| Div => " /")^"\n"
fun emitCall f param = param^" "^f^"\n"
fun emitApp f param = param^" "^f^" execute\n"
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
	TString => e^" type "
	| TInt => e^" . "
	| _ => raise Fail "error interno")^"\n"
fun emitCr() = " cr\n"
fun emitRead e t =
	(case t of
	TInt => "getint to "^e^"\n"
	| TString => "accept to "^e^"\n"
	| _ => raise Fail "error interno")
fun emiteLambda s body = ""
fun emitLet e = " "^e^"\n"
fun emitFunc f (arg:(string*Tipo)) body =
	let	fun emitargs[] = ""
		| emitargs(h::t) = "_"^h^" "^emitargs t
	in
		":noname "^"{ "^(#1 arg)^"}\n"^ body^";\nis "^f^"\n"
	end
