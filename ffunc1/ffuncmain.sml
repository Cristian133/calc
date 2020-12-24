open BasicIO Nonstdio

fun createLexerStream (is : instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
fun elimnl s =
	let	fun aux[] = ""
		| aux((#"\n"::(#"\n"::t))) = "\n"^aux t
		| aux(h::t) = str h^aux t
	in	aux(explode s) end
val flags = {ast=ref false, lift=ref false}
fun opciones [] = []
| opciones("-ast"::t) = (#ast flags:= true; opciones t)
| opciones("-lift"::t) = (#lift flags:= true; opciones t)
| opciones(h::t) = (h::opciones t)
fun main args =
	let	val args' = opciones args
		val entrada = open_in(hd(args'))
					handle Empty => std_in
		val lexbuf = createLexerStream entrada
		val (decs, exp) = ffuncgrm.prog ffunclex.Tok lexbuf
		val _ = if !(#ast flags) then
					(print(ffuncast.ppDecls decs^"---\n");
					print(ffuncast.ppExpr exp^"\n"))
				else ()
		val env = ffuncenv.creaEnv decs (ffuncenv.newenv())
		val (env', decs') = ffunclifting.liftDecs env decs
		val (env'', exp') = ffunclifting.liftExp env' exp
		val _ = if !(#lift flags) then
					(print(ffuncast.ppDecls decs'^"---\n");
					print(ffuncast.ppExpr exp'^"\n"))
				else ()
		val (dfrs, vars) = ffuncenv.gendefers env''
		val (env''', lambdas) = ffunctipado.tipaDecs
							(!ffunclifting.liftedLambdas) env'' []
		val (env4, exps) = ffunctipado.tipaDecs decs env''' []
		val (_, exp'') = ffunctipado.tipaExp exp' env4
		val salida = TextIO.openOut("out.fs")
	in
		TextIO.output(salida, ffuncrt.prol);
		TextIO.output(salida, dfrs^vars);
		TextIO.output(salida, lambdas);
		TextIO.output(salida, elimnl exps);
		TextIO.output(salida, elimnl exp'');
		TextIO.output(salida, "\nbye\n");
		TextIO.closeOut salida
	end
	handle e => print(exnMessage e^"\n")

val it = main(CommandLine.arguments())
