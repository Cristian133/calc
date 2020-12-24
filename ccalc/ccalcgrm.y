%{
open ccalcast
%}
%token EOF PCOMA MAS MENOS POR DIV PIZQ PDER
%token MENOR MENIG MAYOR MAYIG IGUAL DIST ASIGN IF
%token THEN ELSE WHILE DO END PRINT
%token<string> VAR

%token<int> NRO
%type<ccalcast.Expr> expr
%type<ccalcast.Expr> linea

%left PCOMA
%left ELSE
%nonassoc PRINT
%nonassoc ASIGN
%nonassoc MENOR MENIG MAYOR MAYIG IGUAL DIST
%left MAS MENOS
%left POR DIV
%right UMENOS
%start linea
%%
linea: expr EOF 					{ $1 }
	| EOF							{ Nop }
	;
expr: NRO							{ Nro $1 }
	| VAR							{ Var $1 }
	| expr MAS expr					{ Suma($1,$3) }
	| expr MENOS expr				{ Resta($1,$3) }
	| expr POR expr					{ Prod($1,$3) }
	| expr DIV expr					{ Div($1,$3) }
	| expr MENOR expr				{ Cmp(Menor, $1,$3) }
	| expr MENIG expr				{ Cmp(MenIg, $1,$3) }
	| expr MAYOR expr				{ Cmp(Mayor, $1,$3) }
	| expr MAYIG expr				{ Cmp(MayIg, $1,$3) }
	| expr IGUAL expr				{ Cmp(Igual, $1,$3) }
	| expr DIST expr				{ Cmp(Dist, $1,$3) }
	| PIZQ expr PDER				{ $2 }
	| VAR ASIGN expr				{ Asign($1, $3) }
	| MENOS %prec UMENOS expr		{ Opuesto $2 }
	| IF expr THEN expr ELSE expr	{ If($2, $4, $6) }
	| WHILE expr DO expr END		{ While($2, $4) }
	| PRINT expr					{ Print $2 }
	| expr PCOMA expr				{ Sec($1, $3) }
	;
%%
