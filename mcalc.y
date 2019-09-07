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

char *rem(char *l, char *r) {
	char *res = malloc(2*strlen(l)+2*strlen(r)+20);
	sprintf(res, "(- %s (* (/ %s %s) %s))", l, l, r, r);
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
int yylex();
void yyerror(char *);
%}

%union {
	char *val;
}

%token	<val> NUM
%token  ADD SUB MUL DIV IF MOD PRINT OPEN CLOSE
%type	<val> exp 

%left MOD
%left IF
%left ADD SUB
%left MUL DIV
%left NEG

/* Gramatica */
%%

input: 		
		| 		exp     { puts($1);}
		| 		error  	{ fprintf(stderr, "Entrada inválida\n"); }
;

exp: 			NUM 		{ $$ = dup($1); }
		| 		exp ADD exp	{ $$ = oper('+', $1, $3);}
		| 		exp SUB exp	{ $$ = oper('-', $1, $3);}
		| 		exp MUL exp	{ $$ = oper('*', $1, $3);}
		|		exp DIV exp { $$ = oper('/', $1, $3);}
		|		IF exp exp exp { $$ = iffer($2, $3, $4);}
		|		MOD exp exp { $$  = rem($2, $3);     }
		| 		SUB exp %prec NEG  { $$ = oper('~', $2, "");} 
		| 		OPEN exp CLOSE	{ $$ = dup($2);}
;

%%

void yyerror(char *s) {
  fprintf(stderr,"%s\n",s);
}