%{
open ffuncast
%}
%token EOF
%token<int> NRO
%token<string> ID TEXTO
%token<ffunctipo.Tipo> TIPO
%token PI PD MAS MENOS POR DIV COMA PCOMA DOSP ARROBA
%token IGUAL MENOR MAYOR MENIG MAYIG DIST
%token ASIGN IF THEN ELSE PUNTO
%token FUN END PRINT FLECHA
%token READ LET IN CR UNIT

%left LET
%left COMA
%left FLECHA
%left PCOMA PI
%nonassoc IF
%left ELSE
%nonassoc ASIGN
%nonassoc MAYOR MENOR MAYIG MENIG IGUAL DIST
%left MAS MENOS
%left POR DIV
%left PUNTO
%left ID NRO UNIT TEXTO
%left APP

%type<ffuncast.Expr> prog
%type<ffuncast.Decl list> decls
%type<ffuncast.Decl> dec
%type<ffuncast.Expr> exp expf sent
%type<ffuncast.Expr list> params rparams tupla
%type<ffunctipo.Tipo> tipo
%type<ffunctipo.Tipo list> tipos rtipos
%start prog
%%
prog: exp EOF						{ $1 }
	;
decls: dec decls					{ $1::$2 }
	|								{ [] }
	;
dec: PI ID DOSP tipo PD DOSP tipo ARROBA ID IGUAL exp END
							{ Func{f=$9, arg=($2, $4), body=$11, tipo=$7, pr=0} }
	;
tipo: TIPO							{ $1 }
	| tipo FLECHA tipo				{ TF($1, $3) }
	| PI tipos PD					{ TT $2 }
	;
tipos: tipo rtipos					{ $1::$2 }
	;
rtipos: COMA tipo rtipos			{ $2::$3 }
	| 	 	 						{ [] }
	;
exp: NRO							{ Nro $1 }
	| TEXTO							{ Texto $1 }
	| UNIT							{ Unit }
	| expf							{ $1 }
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
	| tupla 						{ Tupla $1 }
	| PI exp PD						{ $2 }
	| exp PCOMA exp					{ Sec($1, $3) }
	| IF exp THEN exp ELSE exp		{ If($2, $4, $6) }
	| LET decls IN exp END			{ Let($2, $4) }
	| sent							{ $1 }
	;
expf : ID	 						{ Id $1 }
	| expf PUNTO NRO				{ Memb($1, $3) }
	| %prec APP exp ARROBA expf			{ Call($3, $1) }
	;
sent : PRINT PI exp PD				{ Print $3 }
	| CR							{ Cr }
	| READ ID						{ Read $2 }
	| exp ASIGN exp					{ Asign($1, $3) }
	;
tupla: PI params PD					{ $2 }
	;
params: exp COMA exp rparams		{ $1::$3::$4 }
	;
rparams: COMA exp rparams			{ $2::$3 }
	| 								{ [] }
	;
%%
