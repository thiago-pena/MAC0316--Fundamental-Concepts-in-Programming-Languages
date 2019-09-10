/* Calculadora infixa */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
char *ifElseOper(char *cond, char *sim, char *nao) {
	char *res = malloc(strlen(cond) + strlen(sim) + strlen(nao) + 9);
	sprintf(res, "(if %s %s %s)", cond, sim, nao);
	return res;
}
char *fun(char *funName, char *arg) {
	char *res = malloc(strlen(funName) + strlen(arg) + 9);
	//printf("teste -> %s\n", funName);
	//printf("teste -> %s\n", arg);
	sprintf(res, "'(call %s %s)", funName, arg);
	return res;
}
int yylex();
void yyerror(char *);
%}

%union {
	char *val;
}

%token	<val> NUM S
%token ADD SUB MUL DIV PRINT OPEN CLOSE IF ELSE ENDIF S_INI S_END CALL
%type <val> exp

%left ADD SUB
%left MUL DIV
%left NEG

/* Gramatica */
%%

input:
		| 		exp     { puts($1);}
		| 		error  	{ fprintf(stderr, "Entrada inválida\n"); }
;


exp:			NUM 				{ $$ = dup($1); }
		| 		exp ADD exp			{ $$ = oper('+', $1, $3);}
		| 		exp SUB exp			{ $$ = oper('-', $1, $3);}
		| 		exp MUL exp			{ $$ = oper('*', $1, $3);}
		| 		exp DIV exp			{ $$ = oper('/', $1, $3);}
		| 		SUB exp %prec NEG  	{ $$ = oper('~', $2, "");}
		| 		OPEN exp CLOSE		{ $$ = dup($2);}
		| 		IF exp S_INI exp S_END ELSE S_INI exp S_END	{ $$ = ifElseOper($2, $4, $8);}
		| 		CALL S OPEN exp CLOSE		{ $$ = fun($2, $4);}
;

%%

void yyerror(char *s) {
  fprintf(stderr,"%s\n",s);
}
