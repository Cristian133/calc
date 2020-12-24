open ffuncast

local
	val n = ref 0
in
	fun newId() = "_f"^Int.toString(!n) before n := !n + 1
end
val tS = Int.toString
fun atoi n = (if n<0 then "-" else "")^tS(Int.abs n)
fun elim2arr s =
	let
		fun aux [] = []
		| aux(#"2":: #"@"::t) = []
		| aux(h::t) = h::aux t
	in	implode(aux(explode s)) end
fun sLink dfprof =
	case dfprof of
	0 => "_sl"
	| 1 => "lp@"
	| _ => " _sl "^atoi(~dfprof-1)^" _slink "
fun slinkcell s t dfprof =
	case dfprof of
	0 => "_"^s^(if t=TString then " 2@ " else "")
	| 1 => raise Fail "error interno 16"
	| _ => " _sl "^atoi(~dfprof-1)^" _slink-cell "
fun slinkcellasign s dfprof =
	case dfprof of
	0 => " to "^elim2arr s^"\n"
	| 1 => raise Fail "error interno 16"
	| _ => " _sl "^atoi(~dfprof-1)^" _slink-cell "
fun gmktp [] cnt = "\n"
| gmktp(TString::t) cnt =
	" -rot _mkstring swap dup rot swap "^tS cnt^" cells + !\n"^gmktp t (cnt+1)
| gmktp((TT lt)::t) cnt =
	("here "^tS(length lt)^" cells allot\n"^gmktp lt 0)^"\n"^gmktp t (cnt+1)
| gmktp(h::t) cnt = " dup rot swap "^tS cnt^" cells + ! "^gmktp t (cnt+1)
fun genmktp lt =
    "here "^tS(length lt)^" cells allot\n"^gmktp lt 0

fun emitNro n = atoi n
fun emitTexto s = "s\" "^s^"\" _mkstring\n"
fun emitUnit() = "0 "
fun emitId s t dp b = slinkcell s t dp
fun emitTupla lt le =
	(List.foldr(fn(e, s) => e^" "^s) "" (rev le))^" "^
			tS(length lt)^" _mktuple\n"


fun emitTuplaRet [] _ = " drop\n"
| emitTuplaRet(TString::t) cnt =
	" dup "^tS cnt^" cells + @ 2@ rot "^emitTuplaRet t (cnt-1)
| emitTuplaRet((TT lt)::t) cnt = " @ "^(emitTuplaRet lt (length lt -1))^emitTuplaRet t (cnt-1)
| emitTuplaRet(_::t) cnt = " dup "^tS cnt^" cells + @ swap "^emitTuplaRet t (cnt-1)




val tS = Int.toString

fun emitTupla Ret lt =
    let fun et [] _ cells = "drop "^tS cells^" roll\n "
            |       et (TString::t) cnt cells =
                    " dup "^tS cnt^" cells + @ 2@ rot\n "^
                    et t (cnt+1)(cells + 2)
            |       et((TT lt)::t) cnt cells =
                    (" dup "^tS cnt^" cells + @ \tupla\n "^
                    et lt 0 cells)^et t (cnt+1) cells
            |       et(_::t)cnt cells =
                    " dup "^tS cnt^" cells + @ swap "^ et t (cnt+1) (cells+1)
    in	et lt 0 0  end

fun roll1 0 = ""
|   roll1 n = tS n ^"roll"

fun roll2 0 = ""
|   roll2 1 = "swap _mkString"
|   roll2 2 = "rot _mkString"
|   roll2 n = tS n ^"roll"^ tS n ^" roll _mkString"

fun despliega (Tint::t) cnt prof =
    roll1 prof^" over "^tS cnt^" cells + !"^ despliega t (cnt+1) prof
|   despliega (TString::t) cnt prof =
    roll2 (prof+1)^" over "^tS cnt^" cells + ! "^ despliega t (cnt+1) prof

|   despliega ((TT lt)::t) cnt prof =
    "here "^tS(length lt)^" cells allot \n"^ despliega lt 0 (prof+1)^" swap dup rot "^tS cnt^" cells + !\n"^ despliega t (cnt+1) prof

|   despliega []_ _ = ""

 
fun emitMemb lt e n t =(* porblema *) 
	let val ef = if t=TString then " @ 2@" else ""
	in
		e^" "^(if n<>0 then tS n^" cells + @ " else "@")^ef^"\n"
	end

fun emitAri(oper, e1, e2) =
			e1^" "^e2^
			(case oper of
			Suma => " +"
			| Resta => " -"
			| Prod => " *"
			| Div => " /")^"\n"

fun emitCall f param tret dp = (* agregar despliega *)
	"here "^sLink dp^" "^param^" "^(elim2arr f)^
	(case tret of
	TString => " _mkstring"
	| TT tl => " "^genmktp (rev tl)
	| _ => "")^"\n"

fun emitSeq e1 e2 = e1^"\n"^e2^"\n"

fun emitAsignTupla cod tp =
	let	fun patch (#"@":: #"2"::t) = if tp=TString then patch t else t
		| patch (#"@"::t) = #"\n":: #"!"::t
		| patch(h::t) = h::patch t
		| patch [] = raise Fail "error interno 15"
	in	implode(rev(patch(rev(explode cod)))) end

fun emitAsign s t dp e =
	let	val sl = slinkcellasign s dp
	in
		case dp of
		0 => e^sl
		| _ => e^" "^sl^" !\n"
	end

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
fun emitPrint e t p =
	(case t of
	TString => e^" type "
	| TInt => e^" . "
	| _ => raise Fail "error interno 1")^"\n"
fun emitCr() = " cr\n"
fun emitRead e dp t =
	(case t of
	TInt => "getint to "^e^"\n"
	| TString => "accept to "^e^"\n"
	| _ => raise Fail "error interno 2")
fun emitLet e = " "^e^"\n"
fun emitFunc f (arg:(string*Tipo)) tr body =
	":noname "^"{ _h _sl _"^(#1 arg)^" }\n"^ body^"\n"^
		(case tr of TT lt => emitTuplaRet (rev lt) (length lt -1) | _ => "")^
		"\n_h here - allot\n;\nis _"^f^"\n"
