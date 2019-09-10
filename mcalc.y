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

char *porc(char *c){
	char *res = malloc(strlen(c) + 25);
	sprintf(res, "(call porcentagem %s)", c);
	return res;
}

char *fat(char *c){
	char *res = malloc(strlen(c) + 25);
	sprintf(res, "(call fatorial %s)", c);
	return res;
}

char *cub(char *c){
	char *res = malloc(strlen(c) + 25);
	sprintf(res, "(call cubo %s)", c);
	return res;
}

char *mpk(char *c){
	char *res = malloc(strlen(c) + 35);
	sprintf(res, "(call milhasParaKm %s)", c);
	return res;
}

char *fpc(char *c){
	char *res = malloc(strlen(c) + 55);
	sprintf(res, "(call fahrentheitParaCelsius %s)", c);
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
%token  PORC IF ADD SUB MUL DIV PRINT OPEN CLOSE MPK FPC FAT CUBO
%type	<val> exp 

/* Precedência */
%left FUNC
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
		|		IF exp exp exp { $$ = iffer($2, $3, $4);}
		| 		exp ADD exp	{ $$ = oper('+', $1, $3);}
		| 		exp SUB exp	{ $$ = oper('-', $1, $3);}
		| 		exp MUL exp	{ $$ = oper('*', $1, $3);}
		|		exp DIV exp { $$ = oper('/', $1, $3);}
		|		PORC exp { $$ = porc($2);}
		|		CUBO exp { $$ = cub($2);}
		|		FAT exp { $$ = fat($2);}
		|		MPK exp { $$ = mpk($2);}
		|		FPC exp { $$ = fpc($2);}
		| 		SUB exp %prec NEG  { $$ = oper('~', $2, "");} 
		| 		OPEN exp CLOSE	{ $$ = dup($2);}
;

%%

void yyerror(char *s) {
  fprintf(stderr,"%s\n",s);
}
