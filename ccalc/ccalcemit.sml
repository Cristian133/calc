open ccalcast

val tabla = Hashset.empty(Hashset.polyHash, op=)

fun emit1(Nro n) =
	if n<0 then "-"^Int.toString(~n) else Int.toString n
| emit1(Var s) =
	if Hashset.member(tabla, s) then s^" @\n"
	else raise Fail (s^" inexistente!\n")
| emit1(Suma(e1, e2)) = emit1 e1^" "^emit1 e2^" +\n"
| emit1(Resta(e1, e2)) = emit1 e1^" "^emit1 e2^" -\n"
| emit1(Prod(e1, e2)) = emit1 e1^" "^emit1 e2^" *\n"
| emit1(Div(e1, e2)) = emit1 e1^" "^emit1 e2^" /\n"
| emit1(Opuesto e) = emit1 e^" negate\n"
| emit1(Cmp(oper, e1, e2)) =
	emit1 e1^"\n"^emit1 e2^"\n"^
	(case oper of
	Menor => "<\n"
	| MenIg => "<=\n"
	| Mayor => ">\n"
	| MayIg=> ">=\n"
	| Igual => "=\n"
	| Dist => "<>\n")
| emit1(Print e) = emit1 e^" . cr\n"
| emit1(Asign(s, e)) =
	let	val _ =
			case Hashset.peek(tabla, s) of
			NONE => Hashset.add(tabla, s)
			| _ => ()
	in
		emit1 e^"\n"^s^" !\n"
	end
| emit1(If(e1, e2, e3)) =
	emit1 e1^" if\n"^emit1 e2^" else \n"^emit1 e3^" endif\n"
| emit1(While(e1, e2)) =
	"begin\n"^emit1 e1^" while\n"^emit1 e2^" repeat\n"
| emit1(Sec(e1, e2)) =
	emit1 e1^emit1 e2
| emit1 Nop = ""

fun emit e =
	let	val s = emit1 e
		val vs = Hashset.listItems tabla
		val vs' = List.foldr (fn(x, y) => "variable "^x^"\n"^y) "" vs
	in	vs'^": main\n"^emit1 e^"\n;\nmain\nbye\n" end
