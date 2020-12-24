{
open ffuncgrm
open ffunctipo

fun atoi s = (valOf o Int.fromString) s
}
rule Tok =
	parse eof		{ EOF }
	| [` ``\t``\n`]		{ Tok lexbuf }
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
	| `.`			{ PUNTO }
	| `=`			{ IGUAL }
	| `<`			{ MENOR }
	| `>`			{ MAYOR }
	| "<="			{ MENIG }
	| ">="			{ MAYIG }
	| "<>"			{ DIST }
	| ":="			{ ASIGN }
	| "->"			{ FLECHA }
	| "entero"		{ TIPO TInt }
	| "cadena"		{ TIPO TString }
	| "unidad"		{ TIPO TUnit }
	| "funcion"		{ FUN }
	| "fin"			{ END }
	| "si"			{ IF }
	| "entonces"		{ THEN }
	| "sino"		{ ELSE }
	| "imprimir"		{ PRINT }
	| "adentro"		{ CR }
	| "leer"		{ READ }
	| "hacer"		{ LET }
	| "en"			{ IN }
	| "@"			{ ARROBA }
	| `"`			{ TEXTO(Text lexbuf) }
	| "()"			{ UNIT }
	| [`a`-`z`]+		{ ID(getLexeme lexbuf) }
	| [`0`-`9`]+		{ NRO(atoi(getLexeme lexbuf)) }
	| _			{ raise Fail ("["^getLexeme lexbuf^"]") }
and Text =
	parse eof		{ raise Fail "string incompleta" }
	| `\n`			{ raise Fail "nl en string" }
	| `"`			{ "" }
	| _			{ getLexeme lexbuf^Text lexbuf }
and Coment =
	parse eof		{ raise Fail "comentario incompleto" }
	| `\n`			{ () }
	| _			{ Coment lexbuf }

	;
