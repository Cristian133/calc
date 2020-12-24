{
open ccalcgrm

fun atoi s = valOf(Int.fromString s)
}

let ID = [`A`-`Z``a`-`z`][`A`-`Z``a`-`z``_``0`-`9`]*

rule Tok =
	parse eof 		{ EOF }
	| [` ``\t``\n`]	{ Tok lexbuf }
	| `;`			{ PCOMA }
	| `+`			{ MAS }
	| `-`			{ MENOS }
	| `*`			{ POR }
	| `/`			{ DIV }
	| `<`			{ MENOR }
	| "<="			{ MENIG }
	| `>`			{ MAYOR }
	| ">="			{ MAYIG }
	| `=`			{ IGUAL }
	| "<>"			{ DIST }
	| `(`			{ PIZQ }
	| `)`			{ PDER }
	| "<-"			{ ASIGN }
	| "if"			{ IF }
	| "then"		{ THEN }
	| "else"		{ ELSE }
	| "while"		{ WHILE }
	| "do"			{ DO }
	| "end"			{ END }
	| "print"		{ PRINT }
	| [`0`-`9`]+	{ NRO(atoi(getLexeme lexbuf)) }
	| `-`[`0`-`9`]+	{ NRO(atoi(getLexeme lexbuf)) }
	| ID			{ VAR(getLexeme lexbuf) }
	;
