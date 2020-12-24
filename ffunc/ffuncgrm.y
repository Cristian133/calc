%{
open ffuncast
%}
%token EOF
%token<int> NRO
%token<string> ID
%token PI PD MAS MENOS POR DIV COMA PCOMA DOSP
%token IGUAL MENOR MAYOR MENIG MAYIG DIST ARRO
%token ASIGN IF THEN ELSE
%token FUN FN END PRINT FLECHA

%left FLECHA
%left PCOMA
%left ELSE
%left ASIGN
%nonassoc PRINT
%nonassoc MAYOR MENOR MAYIG MENIG IGUAL DIST
%left MAS MENOS
%left POR DIV
%left ARRO

%type<ffuncast.Decl list * ffuncast.Expr> prog
%type<ffuncast.Decl list> decls
%type<ffuncast.Decl> dec
%type<ffuncast.Expr> exp
%type<string list> args rargs
%type<ffuncast.Expr list> params rparams
%start prog
%%
prog: decls exp EOF					{ ($1, $2) }
	;
decls: dec decls					{ $1::$2 }
	|								{ [] }
	;
dec: FUN ID PI args PD DOSP exp END	{ Func{func=$2, args=$4, body=$7} }
	;
args: ID rargs						{ $1::$2 }
	|								{ [] }
	;
rargs: COMA ID rargs				{ $2::$3 }
	|								{ [] }
	;
exp: NRO							{ Nro $1 }
	| ID							{ Id $1 }
	| exp MAS exp					{ Suma($1, $3) }
	| exp MENOS exp					{ Resta($1, $3) }
	| exp POR exp					{ Prod($1, $3) }
	| exp DIV exp					{ Div($1, $3) }
	| exp IGUAL exp					{ Cmp(Igual, $1, $3) }
	| exp MENOR exp					{ Cmp(Menor, $1, $3) }
	| exp MENIG exp					{ Cmp(MenIg, $1, $3) }
	| exp MAYOR exp					{ Cmp(Mayor, $1, $3) }
	| exp MAYIG exp					{ Cmp(MayIg, $1, $3) }
	| exp DIST exp					{ Cmp(Dist, $1, $3) }
	| exp ARRO exp					{ App($1, $3) }
	| ID PI params PD				{ Call($1, $3) }
	| PI exp PD						{ $2 }
	| exp PCOMA exp					{ Sec($1, $3) }
	| ID ASIGN exp					{ Asign($1, $3) }
	| IF exp THEN exp ELSE exp		{ If($2, $4, $6) }
	| PRINT exp						{ Print $2 }
	| FN ID FLECHA exp END			{ Lambda($2, $4) }
	;
params: exp rparams					{ $1::$2 }
	|								{ [] }
	;
rparams: COMA exp rparams			{ $2::$3 }
	|								{ [] }
	;
%%
