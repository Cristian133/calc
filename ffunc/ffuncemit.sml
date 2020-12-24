open ffuncast

local
	val n = ref 0
in
	fun newId() = "_f"^Int.toString(!n) before n := !n + 1
end
val ed = ref false
val clausura = ref []
fun emitCl() = List.foldr(fn(v, s) => v^" "^s) "" (hd(!clausura))

fun emitEx(Nro n) =
	(if n<0 then "-" else "")^Int.toString(Int.abs n)
| emitEx(Id s) =
	(case List.find(fn x => x=s) (!ffunclifting.lambdaList) of
	SOME _ => (if !ed then emitCl()^"['] " else "' ")^s | _ => s)
| emitEx Nop = ""
| emitEx(Suma(e1, e2)) = emitEx e1^" "^emitEx e2^" +\n"
| emitEx(Resta(e1, e2)) = emitEx e1^" "^emitEx e2^" -\n"
| emitEx(Prod(e1, e2)) = emitEx e1^" "^emitEx e2^" *\n"
| emitEx(Div(e1, e2)) = emitEx e1^" "^emitEx e2^" /\n"
| emitEx(App(e1, e2)) = emitEx e2^" "^emitEx e1^" execute\n"
| emitEx(Call(s, elst)) =
	(case Polyhash.peek ffuncenv.env s of
	SOME n =>
		if length elst<>n then raise Fail (s^" con args inconvenientes\n")
		else ()
	| _ => raise Fail (s^" inexistente\n");
	(List.foldr (fn(x, y) => emitEx x^" "^y) "" elst)^s^"\n")
| emitEx(Sec(e1, e2)) = emitEx e1^"\n"^emitEx e2
| emitEx(Asign(s, e)) = emitEx e^" to "^s^"\n"
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
| emitEx(Print e) = emitEx e^" . cr\n"
| emitEx(Lambda(s, e)) = raise Fail (s^" sin lifting!!\n")
	(*
	let	val id = newId()
	in
		": "^id^" recursive\n"^"{ "^s^" }\n"
		^emitEx e^" ;\n' "^id^"\n"
	end
	*)
fun emitDecl(Func{func, args, body}) =
	let	fun emitargs[] = ""
		| emitargs(h::t) = h^" "^emitargs t
	in
		clausura:=args:: !clausura;
		": "^func^" recursive\n"^"{ "^emitargs args^"}\n"
		^emitEx body^"\n;\n"
			before clausura:= tl(!clausura)
	end
fun emitDecls [] = ""
| emitDecls(h::t) = emitDecl h^emitDecls t
