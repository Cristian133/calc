%{
open ffuncast
%}
%token EOF
%token<int> NRO
%token<string> ID TEXTO
%token<ffunctipo.Tipo> TIPO
%token PI PD MAS MENOS POR DIV COMA PCOMA DOSP
%token IGUAL MENOR MAYOR MENIG MAYIG DIST ARRO
%token ASIGN IF THEN ELSE
%token FUN FN END PRINT FLECHA
%token READ LET IN CR UNIT

%left COMA
%left FLECHA
%left PCOMA
%left ELSE
%nonassoc ASIGN
%nonassoc PRINT
%nonassoc MAYOR MENOR MAYIG MENIG IGUAL DIST
%left MAS MENOS
%left POR DIV
%left ARRO
%left ID

%type<ffuncast.Decl list * ffuncast.Expr> prog
%type<ffuncast.Decl list> decls
%type<ffuncast.Decl> dec
%type<ffuncast.Expr> exp
%type<string*ffunctipo.Tipo> arg
%type<ffuncast.Expr list> params rparams tupla
%type<ffunctipo.Tipo> tipo
%type<ffunctipo.Tipo list> tipos rtipos
%start prog
%%
prog: decls exp EOF					{ ($1, $2) }
	;
decls: dec decls					{ $1::$2 }
	|								{ [] }
	;
dec: FUN ID PI arg PD DOSP tipo IGUAL exp END
									{ Func{func=$2, arg=$4, body=$9, tipo=$7} }
	;
arg: ID DOSP tipo 					{ ($1, $3) }
	;
tipo: TIPO							{ $1 }
	| tipo FLECHA tipo				{ TF($1, $3) }
	| PI tipos PD					{ TT $2 }
	;
tipos: tipo rtipos					{ $1::$2 }
	| 								{ [] }
	;
rtipos: COMA tipo rtipos			{ $2::$3 }
	| tipo 	 						{ [$1] }
	;
exp: NRO							{ Nro $1 }
	| TEXTO							{ Texto $1 }
	| ID							{ Id $1 }
	| UNIT							{ Unit }
	| exp MAS exp					{ Oper(Suma, $1, $3) }
	| exp MENOS exp					{ Oper(Resta, $1, $3) }
	| exp POR exp					{ Oper(Prod, $1, $3) }
	| exp DIV exp					{ Oper(Div, $1, $3) }
	| exp IGUAL exp					{ Cmp(Igual, $1, $3) }
	| exp MENOR exp					{ Cmp(Menor, $1, $3) }
	| exp MENIG exp					{ Cmp(MenIg, $1, $3) }
	| exp MAYOR exp					{ Cmp(Mayor, $1, $3) }
	| exp MAYIG exp					{ Cmp(MayIg, $1, $3) }
	| exp DIST exp					{ Cmp(Dist, $1, $3) }
	| exp ARRO exp					{ App($1, $3) }
	| tupla 						{ Tupla $1 }
	| ID exp						{ Call($1, $2) }
	| PI exp PD						{ $2 }
	| exp PCOMA exp					{ Sec($1, $3) }
	| ID ASIGN exp					{ Asign($1, $3) }
	| IF exp THEN exp ELSE exp		{ If($2, $4, $6) }
	| PRINT exp						{ Print $2 }
	| CR							{ Cr }
	| READ ID						{ Read $2 }
	| LET decls IN exp END			{ Let($2, $4) }
	| FN ID DOSP tipo FLECHA exp DOSP tipo END
									{ Lambda($2, $4, $6, $8) }
	;
tupla: PI params PD					{ $2 }
params: exp COMA exp rparams		{ $1::$3::$4 }
	;
rparams: COMA exp rparams			{ $2::$3 }
	| 								{ [] }
	;
%%
