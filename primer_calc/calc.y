%{
#include <stdio.h>
extern char *yytext;
#include <stdlib.h>
%}
%union{
	int I;
};
%token NRO
%type<I> expr
%% 
linea: expr ';' {printf("%d\n",$1);YYACCEPT;}
expr: NRO {$$=atoi(yytext);}
    |expr expr '+' {$$=$1+$2;}
    |expr expr '*' {$$=$1*$2;}
;
%%
int yyerror(char *s)
{
	printf("%s\n",s);
	exit(-1);
}
int main ()
{	yyparse();
	return 0;
}
