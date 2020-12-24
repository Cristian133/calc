{
open calcgrm

fun atoi s = valOf(Int.fromString s)
}
rule Tok =
	parse eof 		{ EOF }
	| [` ``\t``\n`]	{ Tok lexbuf }
	| `;`			{ PCOMA }
	| `+`			{ MAS }
	| `-`			{ MENOS }
	| `*`			{ POR }
	| `/`			{ DIV }
	| `(`			{ PIZQ }
	| `)`			{ PDER }
	| [`0`-`9`]+	{ NRO(atoi(getLexeme lexbuf)) }
	| `-`[`0`-`9`]+	{ NRO(atoi(getLexeme lexbuf)) }
	;
