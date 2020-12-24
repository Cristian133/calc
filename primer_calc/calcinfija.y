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
%left '+'
%left '*'
%% 
linea: expr ';' {printf("%d\n",$1);YYACCEPT;}
expr: NRO {$$=atoi(yytext);}
	
    	|expr '+' expr {$$=$1+$3;}
    	|expr '*' expr {$$=$1*$3;}
	|'('expr')'	{$$=$2;}
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
