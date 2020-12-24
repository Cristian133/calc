open BasicIO Nonstdio

fun createLexerStream (is : instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
fun elimnl s =
	let	fun eatSp(#"\n"::t) = eatSp t
		| eatSp(#" "::t) = eatSp t
		| eatSp l = l
		fun aux[] = ""
		| aux(#"\""::t) =
			let	fun buscaCm [] s = (s, [])
				| buscaCm (#"\""::t) s = (s^"\"", t)
				| buscaCm (h::t) s = buscaCm t (s^str h)
				val (s1, s2) = buscaCm t ""
			in	"\""^s1^aux s2 end
		| aux(#"\n"::t) = "\n"^aux(eatSp t)
		| aux(#" "::t) = " "^aux(eatSp t)
		| aux(h::t) = str h^aux t
	in	aux(explode s) end
val flags = {ast=ref false}
fun opciones [] = []
| opciones("-ast"::t) = (#ast flags:= true; opciones t)
| opciones(h::t) = (h::opciones t)
fun main args =
	let	fun popstack salida =
			if not(ffunctipado.vacia()) then (
				TextIO.output(salida, elimnl(ffunctipado.pop()));
				popstack salida
			)
			else ()
		val args' = opciones args
		val entrada = open_in(hd(args'))
					handle Empty => std_in
		val lexbuf = createLexerStream entrada
		val exp = ffuncgrm.prog ffunclex.Tok lexbuf
		val _ = if !(#ast flags) then
					print(ffuncast.ppExpr exp^"\n")
				else ()
		val env = ffuncenv.newenv()
		val (env', exp', _) = ffunctipado.tipaExp exp env 0
		val salida = TextIO.openOut("out.fs")
	in
		TextIO.output(salida, ffuncrt.prol);
		TextIO.output(salida, ffuncenv.gendefers());
		popstack salida;
		TextIO.output(salida, elimnl(exp'^"\nbye\n"));
		TextIO.closeOut salida
	end
	handle e => print(exnMessage e^"\n")

val it = main(CommandLine.arguments())
