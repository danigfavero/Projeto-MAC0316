D [0-9]
E [Ee][+-]?{D}+
F [A-Za-z]+

%{
#include <math.h>
#include "mcalc.tab.h"

%}


%%
{D}*[.]?{D}+{E}?	{ yylval.val = yytext; return NUM; }

[ \t\n]+	/* pula espacos em branco */

#.*		/* comentarios simples */

[(]		{ return OPEN;}
[)]		{ return CLOSE;}
[+]		{ return ADD; }
[-]		{ return SUB; }
[*]		{ return MUL; }
[/]		{ return DIV; }
[~]		{ return NEG; }
(if)    { return IF;  }
(call)  { return CALL;}
(def)   { return DEF; }
(var)   { return VAR; }
{F}     { yylval.val = yytext; return NAME; }

.	{  fprintf(stderr, "Entrada ignorada\n-->%s\n", yytext); }

%%

/* Local Variables: */
/* mode: c */
/* End: */
