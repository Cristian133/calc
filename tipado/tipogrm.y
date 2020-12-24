%{
open tipoast
%}

%token EOF
%token<tipoast.Tipo> TIPO
%token PARI PARD COMA FLECHA IGUAL

%right FLECHA

%type<tipoast.Tipo> tipo linea
%type<tipoast.Tipo list> tipos tipos1
%start linea
%%
linea: tipo EOF				{ $1 }
	;
tipo: TIPO					{ $1 }
	| PARI tipos PARD		{ Tupla $2 }
	| tipo FLECHA tipo		{ Flecha($1, $3) }
	;
tipos: tipo tipos1			{ $1::$2 }
	| 						{ [] }
	;
tipos1: COMA tipo tipos1	{ $2::$3 }
	| tipo					{ [$1] }
	;
%%
