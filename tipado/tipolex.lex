{
open tipogrm
open tipoast
}
rule Tok =
	parse eof				{ EOF }
	| [` ``\t``\n`]+		{ Tok lexbuf }
	| "int"					{ TIPO Int }
	| "str"					{ TIPO String }
	| "unit"				{ TIPO Unit }
	| `(`					{ PARI }
	| `)`					{ PARD }
	| `,`					{ COMA }
	| "->"					{ FLECHA }
	;
