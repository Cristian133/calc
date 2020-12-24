open BasicIO Nonstdio

fun createLexerStream (is : instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
fun elimnl s =
	let	fun aux[] = ""
		| aux((#"\n"::(#"\n"::t))) = aux(#"\n"::t)
		| aux((#" "::(#" "::t))) = aux(#" "::t)
		| aux((#" "::(#"\n"::t))) = aux(#"\n"::t)
		| aux(h::t) = str h^aux t
	in	aux(explode s) end
fun main args =
	let	val entrada = open_in(hd(args))
					handle Empty => std_in
		val lexbuf = createLexerStream entrada
		val (decs, exp) = tfuncgrm.prog tfunclex.Tok lexbuf
		val env = tfuncenv.creaEnv decs (tfuncenv.newenv())
		val (dfrs, vars) = tfuncenv.gendefers env
		val (env', exps) = tfunctipado.tipaDecs decs env []
		val (_, exp') = tfunctipado.tipaExp exp env'
		val salida = TextIO.openOut("out.fs")
	in
		TextIO.output(salida, tfuncrt.prol);
		TextIO.output(salida, dfrs^vars);
		TextIO.output(salida, exps);
		TextIO.output(salida, elimnl exp');
		TextIO.output(salida, "\nbye\n");
		TextIO.closeOut salida
	end
	handle e => print(exnMessage e^"\n")

val it = main(CommandLine.arguments())
