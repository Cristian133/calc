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
		val (decs, exp) = ffuncgrm.prog ffunclex.Tok lexbuf
		val _ = ffuncenv.creaEnv decs
		val _ = print(ffuncast.ppDecls decs^"\n")
		val _ = print(ffuncast.ppExpr exp^"\n")
		val exp' = ffunclifting.liftExp exp
		val decs' = (rev(!ffunclifting.liftedLambdas))@ffunclifting.liftDecs decs
		val _ = print(ffuncast.ppDecls decs'^"\n")
		val _ = print(ffuncast.ppExpr exp'^"\n")
		val salida = TextIO.openOut("out.fs")
	in
		ffuncemit.ed:=true;
		TextIO.output(salida, elimnl(ffuncemit.emitDecls decs'));
		ffuncemit.ed:=false;
		TextIO.output(salida, elimnl(ffuncemit.emitEx exp'^"\nbye\n"));
		TextIO.closeOut salida
	end
	handle e => print(exnMessage e^"\n")

val it = main(CommandLine.arguments())
