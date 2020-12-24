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
		val tipo = tipogrm.linea tipolex.Tok lexbuf
		val _ = print(tipoast.ppTipo tipo^"\n")
	in
		print "Bem!\n"
	end
	handle e => print(exnMessage e^"\n")

val it = main(CommandLine.arguments())
