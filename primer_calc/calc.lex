%{
#include "calc.tab.h"
%}
%%
[" "\n\t]+	;
"+"		return '+';
"*"		return '*';
";"		return ';';
[0-9]+		return NRO;
%%
int yywrap()
{
	return 1;
}
