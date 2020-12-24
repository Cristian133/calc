%{
open tfuncast
%}
%token EOF
%token<int> NRO
%token<string> ID TEXTO
%token PI PD MAS MENOS POR DIV COMA PCOMA DOSP
%token IGUAL MENOR MAYOR MENIG MAYIG DIST
%token ASIGN IF THEN ELSE
%token FUN END PRINT READ CR
%token LET IN
%token<tfunctipo.Tipo> TIPO

%left FLECHA
%left PCOMA
%left ELSE
%left ASIGN
%nonassoc PRINT READ
%nonassoc MAYOR MENOR MAYIG MENIG IGUAL DIST
%left MAS MENOS
%left POR DIV

%type<tfuncast.Decl list * tfuncast.Expr> prog
%type<tfuncast.Decl list> decfs
%type<tfuncast.Decl> dec
%type<tfuncast.Expr> exp
%type<(string*tfunctipo.Tipo) list> args rargs
%type<tfuncast.Expr list> params rparams
%type<tfuncast.Decl list> decvars
%start prog
%%
prog: decfs exp EOF					{ ($1, $2) }
	;
decfs: dec decfs					{ $1::$2 }
	|								{ [] }
	;
dec: FUN ID PI args PD DOSP TIPO IGUAL exp END
									{ Func{f=$2, args=$4, res=$7, body=$9} }
	;
args: ID DOSP TIPO rargs			{ ($1, $3)::$4 }
	|								{ [] }
	;
rargs: COMA ID DOSP TIPO rargs		{ ($2, $4)::$5 }
	|								{ [] }
	;
exp: NRO							{ Nro $1 }
	| TEXTO							{ Texto $1 }
	| ID							{ Id $1 }
	| exp MAS exp					{ Ari(Suma, $1, $3) }
	| exp MENOS exp					{ Ari(Resta, $1, $3) }
	| exp POR exp					{ Ari(Prod, $1, $3) }
	| exp DIV exp					{ Ari(Div, $1, $3) }
	| exp IGUAL exp					{ Cmp(Igual, $1, $3) }
	| exp MENOR exp					{ Cmp(Menor, $1, $3) }
	| exp MENIG exp					{ Cmp(MenIg, $1, $3) }
	| exp MAYOR exp					{ Cmp(Mayor, $1, $3) }
	| exp MAYIG exp					{ Cmp(MayIg, $1, $3) }
	| exp DIST exp					{ Cmp(Dist, $1, $3) }
	| ID PI params PD				{ Call($1, $3) }
	| PI exp PD						{ $2 }
	| exp PCOMA exp					{ Sec($1, $3) }
	| ID ASIGN exp					{ Asign($1, $3) }
	| IF exp THEN exp ELSE exp		{ If($2, $4, $6) }
	| PRINT exp						{ Print $2 }
	| CR							{ Cr }
	| READ exp						{ Read $2 }
	| LET decvars IN exp END		{ Let($2, $4) }
	;
decvars: ID DOSP TIPO decvars		{ Var($1, $3)::$4 }
	|								{ [] }
params: exp rparams					{ $1::$2 }
	|								{ [] }
	;
rparams: COMA exp rparams			{ $2::$3 }
	|								{ [] }
	;
%%
