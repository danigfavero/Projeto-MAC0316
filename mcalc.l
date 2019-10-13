D [0-9]
E [Ee][+-]?{D}+
F [a-zA-Z]+

%{
#include <math.h>
#include "mcalc.tab.h"

%}


%%
{D}*[.]?{D}+{E}?	{ yylval.val = yytext; return NUM; }

[ \t\n]+	/* pula espacos em branco */

#.*		/* comentarios simples */

[(]		{ return OPEN; }
[)]		{ return CLOSE; }
[+]		{ return ADD; }
[-]		{ return SUB; }
[*]		{ return MUL; }
[/]		{ return DIV; }
[~]		{ return NEG; }
(if)    { return IF;  }
(call)  { return CALL;}
(def)   { return DEF; }
{F}     { yylval.val = yytext; return FUNC; }

.	{  fprintf(stderr, "Entrada ignorada\n-->%s\n", yytext); }

%%

/* Local Variables: */
/* mode: c */
/* End: */
