%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "y.tab.h"

#ifdef TEST_PARSER
#include "../../../include/octopus/parser/ast.h"
#else
#include <octopus/parser/ast.h>
#endif

// not in C99 :-(
char* strdup(const char*);
void octyy_error(char *);
%}
%option reentrant
%option extra-type="struct string_buffer *"
%option bison-bridge
%option noyywrap
%option nounput
%option noinput
%option never-interactive

RBRACKET \[
LBRACKET \]
RCURLY \{
LCURLY \}
COLON \:
COMMA ,

ALPHA  [A-Za-z]
LALPHA [a-z]
DIGIT  [0-9]
DIGIT19 [1-9]
DIGITS ({DIGIT})+

ALPHA_NUMERIC {ALPHA}|{DIGIT}
IDENT {LALPHA}({ALPHA_NUMERIC}|[._-])*
VARIABLE [_]

INTEGER {DIGIT}|{DIGIT19}{DIGITS}|-{DIGIT}|-{DIGIT19}{DIGITS}
FRACTION \.{DIGITS}
FLOAT  {INTEGER}{FRACTION}|{FRACTION}
NUMBER {INTEGER}
SCAN [%]{ALPHA}
BOOL  true|false


SPACE ([\ \n\r\t\f])+

UNESCAPEDCHAR [ -!#-\[\]-~]
ESCAPEDCHAR \\['"\\bfnrt/]
CHAR {UNESCAPEDCHAR}|{ESCAPEDCHAR}
CHARS {CHAR}+
QUOTE [']
REGEX_START r{QUOTE}

%x REG
%x OSTR

%%
{BOOL}      {
                yylval->integer = (strcmp(yytext, "true") == 0) ? 1 : 0;
                return BOOL;
            }

{FLOAT}     {
                yylval->dl = atof(yytext);
                return FLOAT;
            }


{IDENT}     { 
                yylval->str = strdup(yytext);
                return IDENT; 
            }

{SCAN}      {
                yylval->c = yytext[1];
                return SCAN;
            }

{VARIABLE}  {
                return VARIABLE;
            }

{REGEX_START}      { yyextra->ptr = yyextra->buffer; BEGIN(REG); }

<REG>\'        { /* saw closing quote - all done */
     BEGIN(INITIAL);
     *yyextra->ptr = '\0';
     yylval->str = strdup(yyextra->buffer);
     return REGEX;
     }

<REG>[^\']+        {
     char *yptr = yytext;

     while ( *yptr )
             *yyextra->ptr++ = *yptr++;
     }


\'     { yyextra->ptr = yyextra->buffer; BEGIN(OSTR); }

<OSTR>\'        { /* saw closing quote - all done */
     BEGIN(INITIAL);
     *yyextra->ptr = '\0';
     yylval->str = strdup(yyextra->buffer);
     return STRING;
     }

<OSTR>[^\']+        {
     char *yptr = yytext;

     while ( *yptr )
             *yyextra->ptr++ = *yptr++;
     }

{NUMBER}   {
                yylval->integer = atoll(yytext);
                return NUMBER;
            }

{RCURLY}        return RCURLY; 
{LCURLY}        return LCURLY;
{RBRACKET}      return RBRACKET;
{COMMA}         return COMMA;
{LBRACKET}      return LBRACKET; 
{COLON}         return COLON;

">"             return GT;
">="            return GE;
"<"             return LT;
"<="            return LE;
"=="            return EQ;
"!="            return NE;


\n          { }
{SPACE}     { /* eat up whitespace */ }

<<EOF>>     return END_OF_INPUT;

.           {
                octyy_error("Unexpected input!");
            }
            
%%
