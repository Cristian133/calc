%token EOF FUN etc
%token <string> ID
%token <int>NRO




%type <funcast.Decl> dec
%type <funcast.Decl list> decls

%start prog
%%
prog : decls expr EOF
	 ;
decls : dec decls								{$1::$2}
	  |											{[]}
	  ;

dec : FUN ID PI args PD DOSP exp END 			{func{func=$2, args=$4, body=$7}}
	;

args : ID rargs 								{$1::$2}
	 |											{[]}
	 ;
rargs : COMA ID rargs 							{$2::$3}
	 |											{[]}
	 ;

