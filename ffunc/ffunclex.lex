{
open ffuncgrm

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
	| `@`			{ ARRO }
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
	| "->"			{ FLECHA }
	| "fun"			{ FUN }
	| "fn"			{ FN }
	| "end"			{ END }
	| "if"			{ IF }
	| "then"		{ THEN }
	| "else"		{ ELSE }
	| "print"		{ PRINT }
	| [`a`-`z`]+	{ ID(getLexeme lexbuf) }
	| [`0`-`9`]+	{ NRO(atoi(getLexeme lexbuf)) }
	| _				{ raise Fail ("["^getLexeme lexbuf^"]") }
	;


