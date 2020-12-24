%{
open calcast
%}
%token EOF PCOMA MAS MENOS POR DIV PIZQ PDER
%token<int> NRO
%type<calcast.Expr> expr
%type<calcast.Expr> linea

%left MAS MENOS
%left POR DIV
%right UMENOS
%start linea
%%
linea: expr PCOMA					{ Print $1 }
	| EOF							{ Nop }
	;
expr: NRO							{ Nro $1 }
	| expr MAS expr					{ Suma($1,$3) }
	| expr MENOS expr				{ Resta($1,$3) }
	| expr POR expr					{ Prod($1,$3) }
	| expr DIV expr					{ Div($1,$3) }
	| PIZQ expr PDER				{ $2 }
	| MENOS %prec UMENOS expr		{ Opuesto $2 }
	;
%%
