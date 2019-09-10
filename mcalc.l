DIGIT   [0-9]
E   [Ee][+-]?{DIGIT}+
NUMBER ({DIGIT}+[.]?{DIGIT}*|{DIGIT}*[.]?{DIGIT}+){E}?
NAME  [a-z][a-z0-9_]+

/* NUMBER: Reconhece tanto '3.' quanto '.3', por exemplo. */


%{
#include <math.h>
#include <stdlib.h> // teste
#include <string.h> // teste
#include "mcalc.tab.h"

%}


%%
if		{ return IF; }
else	{ return ELSE; }
endif	{ return ENDIF; }
call    { return CALL; }
{NAME}          {   char *t = malloc(strlen(yytext) + 1);
                    yylval.val = strcpy(t, yytext);
                    return S;   }
{NUMBER}      	{ yylval.val = yytext; return NUM; }



[ \t\n]+	/* pula espacos em branco */

#.*		/* comentarios simples */

[(]		{ return OPEN; }
[)]		{ return CLOSE; }
[+]		{ return ADD; }
[-]		{ return SUB; }
[*]		{ return MUL; }
[/]		{ return DIV; }
[{]     { return S_INI; }
[}]     { return S_END; }



.	{  fprintf(stderr, "Entrada ignorada\n-->%s\n", yytext); }

%%

/* Local Variables: */
/* mode: c */
/* End: */
