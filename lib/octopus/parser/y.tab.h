
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     RBRACKET = 258,
     LBRACKET = 259,
     RCURLY = 260,
     LCURLY = 261,
     COLON = 262,
     COMMA = 263,
     GT = 264,
     GE = 265,
     LT = 266,
     LE = 267,
     EQ = 268,
     NE = 269,
     VARIABLE = 270,
     EUNEXPECTED = 271,
     END_OF_INPUT = 272,
     BOOL = 273,
     FLOAT = 274,
     NUMBER = 275,
     IDENT = 276,
     REGEX = 277,
     STRING = 278,
     SCAN = 279
   };
#endif
/* Tokens.  */
#define RBRACKET 258
#define LBRACKET 259
#define RCURLY 260
#define LCURLY 261
#define COLON 262
#define COMMA 263
#define GT 264
#define GE 265
#define LT 266
#define LE 267
#define EQ 268
#define NE 269
#define VARIABLE 270
#define EUNEXPECTED 271
#define END_OF_INPUT 272
#define BOOL 273
#define FLOAT 274
#define NUMBER 275
#define IDENT 276
#define REGEX 277
#define STRING 278
#define SCAN 279




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 24 "parse.y"

    long long int integer;
    double dl;
    char* str;
    char c;
    struct ast_object* nPtr;



/* Line 1676 of yacc.c  */
#line 110 "y.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif




