open ccalclex
open ccalcgrm
open BasicIO Nonstdio

fun lexstream(is: instream) =
        Lexing.createLexer(fn b => fn n => buff_input is b 0 n)

fun main args =
	let	val entrada = open_in(hd args) handle Empty => std_in
		val lexbuf = lexstream entrada
		val expr = (linea Tok lexbuf)
					handle _ => (print("Error en parsing! "^
        						"["^(Lexing.getLexeme lexbuf)^"]\n");
								raise Fail "fin!")
		val salida = TextIO.openOut "out.fs"
	in
		TextIO.output(salida, ccalcemit.emit expr);
		TextIO.closeOut salida
	end

val _ = main(CommandLine.arguments())
