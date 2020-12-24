{
open funcgrm

fun atoi s = (valOf o Int.fromString) s
}
rule Tok =
	parse eof		{ EOF }
	| [` ``\t``\n`]	{ Tok lexbuf }
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
	| [`a`-`z`]+	{ ID(getLexeme lexbuf) }
	| [`0`-`9`]+	{ NRO(atoi(getLexeme lexbuf)) }
	| _				{ raise Fail ("["^getLexeme lexbuf^"]") }
	;


