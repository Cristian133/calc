{
open tfuncgrm
open tfunctipo

fun atoi s = (valOf o Int.fromString) s
fun str lexbuf =
	let	val s = getLexeme lexbuf
	in	String.substring(s, 1, size s - 2) end
}
let STR = [`@`-`~`]*
rule Tok =
	parse eof		{ EOF }
	| [` ``\t``\n`]	{ Tok lexbuf }
	| `!`			{ Coment lexbuf; Tok lexbuf }
	| `(`			{ PI }
	| `)`			{ PD }
	| `+`			{ MAS }
	| `-`			{ MENOS }
	| `*`			{ POR }
	| `/`			{ DIV }
	| `,`			{ COMA }
	| `;`			{ PCOMA }
	| `:`			{ DOSP }
	| `=`			{ IGUAL }
	| `<`			{ MENOR }
	| `>`			{ MAYOR }
	| "<="			{ MENIG }
	| ">="			{ MAYIG }
	| "<>"			{ DIST }
	| ":="			{ ASIGN }
	| "fun"			{ FUN }
	| "end"			{ END }
	| "if"			{ IF }
	| "then"		{ THEN }
	| "else"		{ ELSE }
	| "print"		{ PRINT }
	| "cr"			{ CR }
	| "read"		{ READ }
	| "let"			{ LET }
	| "in"			{ IN }
	| "int"			{ TIPO Int }
	| "str"			{ TIPO String }
	| [`a`-`z`]+	{ ID(getLexeme lexbuf) }
	| [`0`-`9`]+	{ NRO(atoi(getLexeme lexbuf)) }
	| `"` STR `"`	{ TEXTO(str lexbuf) }
	| _				{ raise Fail ("["^getLexeme lexbuf^"]") }
and Coment =
	parse eof		{ raise Fail "comentario incompleto" }
	| `\n`			{ () }
	| _				{ Coment lexbuf }
	;
