/* Calculadora infixa */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *iffer(char *c, char *y, char *n) {
	char *res = malloc(strlen(c)+strlen(y)+strlen(n)+6);
	sprintf(res, "(if %s %s %s)", c, y, n);
	return res;
}

char *deefer(char *f, char *x, char *b, char *d) {
	char *res = malloc(2*strlen(f) + strlen(x) + strlen(b) + strlen(d) + 35);
	sprintf(res, "(def %s 1729 (seq (:= %s (func %s %s)) %s))", f, f, x, b, d);
	return res;
}

char *oper(char op, char *l, char *r) {
	char *res = malloc(strlen(l)+strlen(r)+6);
	sprintf(res, "(%c %s %s)", op, l, r);
	return res;
}
char *dup(char *orig) {
	char *res = malloc(strlen(orig)+1);
	strcpy(res,orig);
	return res;
}

char *caller(char *f, char *x) {
	char *res = malloc(strlen(f) + strlen(x) + 10);
	sprintf(res, "(call %s %s)", f, x);
	return res;

}

int yylex();
void yyerror(char *);
%}

%union {
	char *val;
}

%token	<val> NUM NAME
%token  IF ADD SUB MUL DIV OPEN CLOSE DEF CALL NEG 
%type	<val> exp 

/* Precedência */
%left DEF CALL 
%left IF
%left ADD SUB
%left MUL DIV
%left NEG

/* Gramatica */
/* As operações em nossa gramática (soma, subtração, multiplicação, divisão)
são lidas como em uma calculadora comum  x + y 
Nosso if é lido no formato     			 if condição true false
Nossas funções são lidas no formato 	 função arg
*/
%%

input: 		
		| 		exp     { puts($1);}
		| 		error  	{ fprintf(stderr, "Entrada inválida\n"); }
;

exp: 			NUM 		{ $$ = dup($1); }
		|		DEF NAME exp exp exp { $$ = deefer($2, $3, $4, $5);}
		|		CALL NAME exp { $$ = caller($2, $3);}
		|		IF exp exp exp { $$ = iffer($2, $3, $4);}
		| 		exp ADD exp	{ $$ = oper('+', $1, $3);}
		| 		exp SUB exp	{ $$ = oper('-', $1, $3);}
		| 		exp MUL exp	{ $$ = oper('*', $1, $3);}
		|		exp DIV exp { $$ = oper('/', $1, $3);}
		| 		SUB exp %prec NEG  { $$ = oper('~', $2, "");} 
		| 		OPEN NAME CLOSE	{ $$ = dup($2);}
;

%%

void yyerror(char *s) {
  fprintf(stderr,"%s\n",s);
}
