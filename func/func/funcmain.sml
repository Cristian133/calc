open BasicIO Nonstdio

fun createLexerStream (is : instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
fun elimnl s =
	let	fun aux[] = ""
		| aux((#"\n"::(#"\n"::t))) = "\n"^aux t
		| aux(h::t) = str h^aux t
	in	aux(explode s) end
fun main args =
	let	val entrada = open_in(hd(args))
					handle Empty => std_in
		val lexbuf = createLexerStream entrada
		val (decs, exp) = funcgrm.prog funclex.Tok lexbuf
		val _ = funcenv.creaEnv decs
		val salida = TextIO.openOut("out.fs")
	in
		TextIO.output(salida, funcrt.prol);
		TextIO.output(salida, elimnl(funcemit.emitDecls decs));
		TextIO.output(salida, elimnl(funcemit.emitEx exp^"\nbye\n"));
		TextIO.closeOut salida
	end
	handle e => print(exnMessage e^"\n")

val it = main(CommandLine.arguments())
